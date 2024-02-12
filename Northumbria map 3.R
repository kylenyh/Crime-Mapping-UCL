# Task 3 -----------------------------------------------------------------------
# Load packages ----------------------------------------------------------------
library(ggspatial)
library(sf)
library(tidyverse)
library(readxl)

# Load data --------------------------------------------------------------------
asb_data <- read_tsv("https://mpjashby.github.io/crimemappingdata/northumbria_asb.tab") |>
  # Harmonise column names
  janitor::clean_names() |>
  # Convert to an SF object
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  st_transform("EPSG:27700")

# Load neighbourhood boundaries
authority_boundaries <- st_read("https://mpjashby.github.io/crimemappingdata/northumbria_districts.geojson") |>
  st_transform("EPSG:27700")

# Load ward boundaries for Northumbria
ward_boundaries <- st_read("https://mpjashby.github.io/crimemappingdata/northumbria_wards.gpkg")

# Load ward-level population estimates for Northumbria
temp_file <- tempfile(fileext = ".xlsx")

# Download the Excel file and store it in the temporary location
download.file(
  url = "https://mpjashby.github.io/crimemappingdata/northumbria_ward_pop.xlsx",
  destfile = temp_file,
  mode = "wb"
)

ward_pop <- read_excel(temp_file, sheet = "Sheet1")

# Calculate total incidents per local authority district
district_incidents <- asb_data |>
  st_join(authority_boundaries) |>
  group_by(district)|>
  summarise(total_incidents = n())

# Identify the district with the highest number of incidents
highest_district <- district_incidents |>
  filter(total_incidents == max(total_incidents)) |>
  pull(district)

# Filter data for the highest incident district
highest_district_data <- asb_data |>
  st_join(authority_boundaries) |>
  filter(district == highest_district)

# Calculate density within the highest incident district
highest_district_density <- highest_district_data |>
  hotspot_kde(bandwidth_adjust = 0.5) |>
  st_intersection(authority_boundaries)

# Create map for the highest incident district
highest_district_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(aes(fill = kde), data = highest_district_density, colour = NA) +
  scale_fill_distiller(
    palette = "Burg",
    breaks = range(pull(highest_district_density, "kde")),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  geom_sf(data = authority_boundaries, colour = "black", fill = NA) +
  geom_sf_label(
    aes(label = str_wrap(district, 10)),
    data = authority_boundaries,
    alpha = 0.5,
    colour = "black",
    lineheight = 1,
    size = 2.5,
    label.padding = unit(0.1, "lines"),
    label.size = NA
  ) +
  annotation_scale(width_hint = 0.1, style = "ticks", location = "br") +
  labs(
    title = str_glue("Concentration of anti-social behaviour in {highest_district} in 2022"),
    subtitle = str_glue("KDE map showing concentration of anti-social behaviour in {highest_district} in 2022"),
    fill = "Kernel Density of\nAnti-social Behaviour"
  ) +
  theme_void() +
  theme(legend.position = "right")

## Save map 3 --------------------------------------------------------------------
ggsave(
  filename = "highest_incident_district_map.jpg",
  plot = highest_district_map,
  width = 210,
  height = 297,
  units = "mm"
)



