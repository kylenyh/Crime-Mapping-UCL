# Task 1 -----------------------------------------------------------------------
# Load packages
library(ggspatial)
library(sf)
library(sfhotspot)
library(tidyverse)


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


# Calculate the density of anti-social behaviour incidents
asb_density_clip <- asb_data |>
  # Calculate density, using half default bandwidth
  hotspot_kde(bandwidth_adjust = 0.5) |>
  # Clip density layer to local authority boundaries
  st_intersection(authority_boundaries)


# Map 1 ------------------------------------------------------------------------
asb_map <- ggplot() +
  # Add base map
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  # Add KDE layer for anti-social behaviour density
  geom_sf(aes(fill = kde), data = asb_density_clip, colour = NA) +
  # Add KDE color key legend
  scale_fill_distiller(
    palette = "PuRd",
    breaks = range(pull(asb_density_clip, "kde")),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  # Add local authority outlines
  geom_sf(data = authority_boundaries, colour = "black", fill = NA) +
  # Add local authority names
  geom_sf_label(
    aes(label = str_wrap(district, 10)),
    data = authority_boundaries,
    # Make labels semi-transparent
    alpha = 0.5,
    # Set label colour
    colour = "black",
    # Reduce space between lines on labels with multiple lines
    lineheight = 1,
    # Reduce label font size
    size = 2.5,
    # Reduce padding around labels
    label.padding = unit(0.1, "lines"),
    # Remove border around labels
    label.size = NA
  ) +
  # Add scale bar indicating distance in kilometers
  annotation_scale(width_hint = 0.1, style = "ticks", location = "br") +
  # Add title and caption
  labs(
    title = str_glue(
      "Concentration of anti-social behaviour at Northumbria in 2022"
    ),
    subtitle = str_glue(
      "KDE map showing concentration of anti-social behaviour at Northumbria in 2022"
    ),
    fill = "kernel\ndensity of\nanti-social behaviour"
  ) +
  # Remove contextual elements from the map
  theme_void() +
  # Adjust legend position
  theme(legend.position = "right")

## Save map 1 --------------------------------------------------------------------
ggsave(
  "asb_density_map.jpg",
  plot = asb_map,
  width = 210,
  height = 297,
  units = "mm"
)
