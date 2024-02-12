
## Load packages and data ------------------------------------------------------

# Note: both datasets will be harmonised to use EPSG:6538 for use with
# `hotspot_kde()`

# Load packages
library(ggspatial)
library(sf)
library(sfhotspot)
library(tidyverse)

# Load shootings data
shootings <- read_csv("https://mpjashby.github.io/crimemappingdata/nyc_shootings.csv") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  # Filter just those shootings that were fatal
  filter(murder == TRUE) |>
  st_transform("EPSG:6538")

# Load NYC police precincts data
precincts <- read_sf("https://mpjashby.github.io/crimemappingdata/nyc_precincts.gpkg") |>
  janitor::clean_names() |>
  # Filter out precincts in Staten Island, since there are very few fatal
  # shootings there and it takes up a lot of space due to the position of Staten
  # Island relative to the rest of NYC
  filter(precinct <= 119) |>
  st_transform("EPSG:6538")



## Wrangle data ----------------------------------------------------------------

# Calculate KDE
shootings_kde <- shootings |>
  hotspot_kde(
    grid = hotspot_grid(precincts, cell_size = 250),
    bandwidth_adjust = 0.33,
    quiet = TRUE
  ) |>
  st_intersection(precincts)



## Make map --------------------------------------------------------------------

shootings_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0, progress = "none") +
  geom_sf(aes(fill = kde), data = shootings_kde, alpha = 0.75, colour = NA) +
  geom_sf(data = precincts, colour = "grey33", fill = NA) +
  geom_sf_label(
    aes(label = scales::ordinal(precinct)),
    data = precincts,
    alpha = 0.5,
    colour = "grey33",
    size = 2.5,
    label.size = NA
  ) +
  annotation_scale(width_hint = 1/5, style = "ticks", location = "br") +
  scale_fill_distiller(
    palette = "Reds",
    breaks = range(pull(shootings_kde, "kde")),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  labs(
    title = str_glue(
      "Fatal shootings in NYC concentrate in Crown Heights and Harlem"
    ),
    subtitle = str_glue(
      "Fatal shootings recorded by NYC Police, 2019, excluding Staten Island"
    ),
    caption = str_glue(
      "Author: Joe Bloggs, Date produced: {lubridate::today()},\n",
      "Data: https://data.cityofnewyork.us/d/833y-fsy8 and OpenStreetMap"
    ),
    fill = "kernel\ndensity of\nshootings"
  ) +
  theme_void() +
  theme(
    # Since we're going to overlay the legend on the map, we should give the
    # legend a solid background
    legend.background = element_rect(colour = NA, fill = "#F2F2F2"),
    # Use the top-right corner of the legend as the anchor point for determining
    # its position
    legend.justification = c(1, 1),
    # Make the legend colour bar smaller
    legend.key.width = unit(0.8, "lines"),
    # Create a margin around the legend, since it has a solid background
    legend.margin = margin(6, 6, 6, 6),
    # Move the top-right corner of the legend to be very near the top-right
    # corner of the map
    legend.position = c(0.98, 0.98),
    # Make the legend and title smaller to reduce their visual prominence
    legend.text = element_text(size = rel(0.7)),
    legend.title = element_text(size = rel(0.8)),
    plot.subtitle = element_text(size = rel(0.8), margin = margin(3, 0, 6, 0)),
    plot.caption = element_text(colour = "grey67", size = rel(0.7), hjust = 0)
  )


