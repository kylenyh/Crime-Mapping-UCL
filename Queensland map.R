
# Load packages --------------------------------------------------------------------
library(ggspatial)
library(sf)
library(tidyverse)

# Load data --------------------------------------------------------------------

stalking_file <- tempfile(fileext = ".xlsx")
download.file(
  url = "https://mpjashby.github.io/crimemappingdata/qld_stalking.xlsx",
  destfile = stalking_file
)
# We only want to map 2018 data but the data file contains data for lots of 
# years, so we will filter this data now
stalking <- stalking_file |> readxl::read_excel() |> filter(year == 2018)

population <- read_csv("https://mpjashby.github.io/crimemappingdata/qld_population.csv.gz")
divisions <- read_sf("https://mpjashby.github.io/crimemappingdata/qld_police_divisions.gpkg")


# Wrangle data -----------------------------------------------------------------

# Join data and calculate rate
stalking_rate <- divisions |> 
  left_join(population, by = c("division" = "police_division")) |> 
  left_join(stalking, by = "division") |> 
  mutate(rate = stalking / (population / 10^5))

# Find divisions with highest stalking rate and label them in order
stalking_rate_highest <- stalking_rate |> 
  slice_max(rate, n = 10) |> 
  mutate(label = str_glue("{division} ({scales::ordinal(row_number())})"))


# Create map -------------------------------------------------------------------

stalking_map <- ggplot() +
  # Base map
  annotation_map_tile(type = "cartolight", zoomin = 1, progress = "none") +
  # Police divisions
  geom_sf(
    aes(fill = rate),
    data = stalking_rate,
    inherit.aes = FALSE,
    alpha = 0.75,
    size = 0.25
  ) +
  # 10 divisions with highest rates
  geom_sf(
    data = stalking_rate_highest,
    inherit.aes = FALSE,
    colour = "black",
    fill = NA,
    size = 0.75
  ) +
  # Labels for 10 divsions with highest rates
  geom_sf_label(
    aes(label = str_wrap(label, width = 12)),
    data = stalking_rate_highest,
    inherit.aes = FALSE,
    alpha = 0.75,
    size = 2.5,
    lineheight = 0.95,
    label.size = NA,
    label.padding = unit(0.1, "lines")
  ) +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  labs(
    title = "Stalking risk is highest in some rural QLD areas",
    subtitle = str_glue(
      "Ten police divisions with the highest stalking incidence are ",
      "highlighted"
    ),
    caption = str_glue(
      "Map produced by Matt Ashby on 31 Jan 2024\nCrime data from ",
      "https://www.police.qld.gov.au/maps-and-statistics"
    ),
    fill = "stalking incidents\nper 10,000 people\nin 2018"
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
