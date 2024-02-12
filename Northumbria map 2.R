# Task 2 -----------------------------------------------------------------------
# Load packages
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


# Wrangle data -----------------------------------------------------------------

# Join ward boundaries with population data
asb_rate <- ward_boundaries |>
  st_join(asb_data) |>
  left_join(ward_pop, by = c("ward_code" = "gss_code")) |>
  mutate(rate = (population / 100000))


# Find wards with the highest incidence of anti-social behaviour
asb_rate_highest <- asb_rate |>
  slice_max(rate, n = 10) |>
  mutate(label = str_glue("{ward_name} ({row_number()})"))

# Map 2 ------------------------------------------------------------------------
asb_ward_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(
    aes(fill = rate),
    data = ward_boundaries,
    colour = NA
  ) +
  geom_sf(
    data = asb_rate_highest,
    inherit.aes = FALSE,
    colour = "black", fill = NA,
    size = 0.75
  ) +
  geom_sf_label(
    aes(label = str_wrap(label, width = 12)),
    data = asb_rate_highest,
    inherit.aes = FALSE,
    alpha = 0.75,
    size = 2.5,
    lineheight = 0.95,
    label.size = NA,
    label.padding = unit(0.1, "lines")
  ) +
  labs(
    title = str_glue(
      "Wards with Highest Incidence of Anti-Social Behaviour"
    ),
    subtitle = str_glue(
      "Top 10 Wards with the Highest Rate of Anti-Social Behaviour Incidents per 10,000 People"
    ),
    caption = str_glue(
      "Data source: Northumbria Anti-Social Behaviour Incidents 2022"
    ),
    fill = "ASB Incidents per 10,000 People"
  ) +
  theme_void() +
  theme(
    # Slightly lower the legend in the visual hierarchy by giving it a
    # background that is white and semi-transparent
    legend.background = element_rect(colour = NA, fill = rgb(1, 1, 1, 0.75)),
    # Base legend position on the top-right corner of the legend, which makes
    # moving the legend to the top-right corner easier
    legend.justification = c(1, 1),
    # Add some padding around the legend
    legend.margin = margin(6, 9, 9, 9),
    # Move the legend almost to the top-right corner of the map, since this is
    # the area of the map with no content in it
    legend.position = c(0.95, 0.98),
    # Make the legend title slightly smaller, again reducing its prominence
    legend.title = element_text(size = 10),
    # Left-align the caption
    plot.caption = element_text(hjust = 0),
    # Add some space above and below the plot subtitle
    plot.subtitle = element_text(margin = margin(6, 0, 6, 0)),
    # Make the plot title larger and bold to move it up the visual hierarchy
    plot.title = element_text(face = "bold", size = 16)
  )


## Save map 2 --------------------------------------------------------------------
ggsave(
  filename = "ward_asb_map.jpg",
  plot = asb_ward_map,
  width = 2000,
  height = 2000,
  units = "px"
)
