# Load necessary packages
library(ggspatial)  # For spatial layers in ggplot2
library(sf)          # For spatial data manipulation
library(sfhotspot)   # For hotspot analysis
library(tidyverse)   # For data manipulation and visualization

# Load data for thefts from vehicles
vehicle_thefts <- read_csv("https://mpjashby.github.io/crimemappingdata/vancouver_thefts.csv.gz") |> 
  janitor::clean_names() |> 
  st_as_sf(coords = c("x", "y"), crs = "EPSG:32610") |> 
  filter(type == "Theft from Vehicle")  # Filter data for thefts from vehicles

# Load Vancouver neighbourhood boundaries
nbhds <- st_read("https://mpjashby.github.io/crimemappingdata/vancouver_neighbourhoods.geojson")

# Create KDE layer for thefts from vehicles
vehicle_theft_density_clip <- vehicle_thefts |> 
  # Calculate density using KDE (Kernel Density Estimation)
  hotspot_kde(bandwidth_adjust = 0.5, quiet = TRUE) |> 
  # Transform the density data to use the same CRS as the neighbourhoods layer
  st_transform("EPSG:4326") |> 
  # Clip the density layer to the area for which we have data (neighbourhoods)
  st_intersection(nbhds)

# Plot map
ggplot() +
  # Add base map
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  # Add density layer for thefts from vehicles
  geom_sf(
    aes(fill = kde), 
    data = vehicle_theft_density_clip, 
    alpha = 0.75, 
    colour = NA
  ) +
  # Add neighbourhood boundaries (with green color and no fill)
  geom_sf(data = nbhds, colour = "red", fill = NA) +
  # Add neighbourhood names with label wrapping and styling
  geom_sf_label(
    aes(label = str_wrap(name, 10)), 
    data = nbhds, 
    alpha = 0.5,
    colour = "red", 
    lineheight = 1, 
    size = 2.5,
    label.size = NA
  ) +
  # Set the color scale for density
  scale_fill_distiller(direction = 1) +
  # Remove unnecessary elements from the map
  theme_void()
