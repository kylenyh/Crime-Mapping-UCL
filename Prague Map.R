# Load packages ----

library(ggspatial)  
library(sf)          
library(sfhotspot)  
library(tidyverse)  



# Load data ----

two_wheeled_vehicle_theft <- read_sf("https://mpjashby.github.io/crimemappingdata/czechia_mcycle_thefts.gpkg") |>
  st_transform("EPSG:8044")

# Creating a temporary file location to download the file
prague_boundary_file <- tempfile(fileext = ".zip")
download.file(
  url = "https://mpjashby.github.io/crimemappingdata/praha_boundary.zip", 
  destfile = prague_boundary_file
)

# A file directory for the prague boundary file 
prague_boundary_dir <- str_glue("{tempdir()}/praha_boundary")

# Unzipping the prague boundary file 
unzip(prague_boundary_file, exdir = prague_boundary_dir)

# Reading through the prague boundary file 
prague_boundary <- prague_boundary_dir |> 
  str_glue("/praha_boundary.shp") |> 
  read_sf() |>
  st_transform("EPSG:8044")
  

# Calculate Gi* statistic ----

theft_gi <- two_wheeled_vehicle_theft |>
  st_intersection(prague_boundary) |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(prague_boundary) 


# Plot map ----

ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  # Add density for significant cells
  geom_sf(
    aes(fill = kde), 
    data = theft_gi, # specifying the dataset containing density information
    alpha = 0.8,
    colour = NA
  ) +
  # Add ward boundaries
  geom_sf(data = prague_boundary, colour = "grey70", fill = NA) + # specifying the dataset containing ward boundaries
  scale_fill_distiller(
    breaks = range(pull(theft_gi, kde)),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  fixed_plot_aspect() +
  labs(
    title = "Prague Two-wheeled Vehicle Theft Hotspots",
    subtitle = "Density of theft in places with more occurrences than expected by chance",
    # Don't forget to add the license statement
    caption = "Contains public sector information licensed under the Open Government Licence v3.0. Map data from OpenStreetMap.",
    fill = "Kernal\ndensity of\ntwo-wheeled vehicle theft"
  ) +
  theme_void() +
  theme(
    plot.caption = element_text(colour = "grey40", hjust = 0),
    plot.subtitle = element_text(margin = margin(t = 6, b = 6)),
    plot.title = element_text(colour = "grey50", face = "bold", size = 16)
  )
