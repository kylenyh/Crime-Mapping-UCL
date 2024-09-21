Introduction

This report outlines the strategic recommendations to the South West BCU police patrol team by presenting the prevailing crime types, crime hotspots and certain crime hotspots.

Using the crime dataset based on the South West BCU boroughs, this report will include a table, charts and maps, followed by an analysis of certain crime hotspots and key locations, to assure safety and security in the South West BCU area.

{r include=FALSE}

# Load packages

library(ggspatial)  
library(sf)          
library(sfhotspot)  
library(gt)
library(tidyverse)

{r include=FALSE}


# Creating a temporary file location to download the file
london_crimes_file <- tempfile(fileext = ".zip")
download.file(
  url = "https://mpjashby.github.io/crimemappingdata/london_crimes.zip", 
  destfile = london_crimes_file,
)

# A file directory for the london crimes file 
london_crimes_dir <- str_glue("{tempdir()}/crimes_in_london")

# Unzipping the london borough file 
unzip(london_crimes_file, exdir = london_crimes_dir)

# Load london boundary file
london_crimes <- london_crimes_dir |>
  str_glue("/london_crimes.csv") |>
  # reads excel file 
  read_csv() |>
  # filters specific types of crimes 
  filter(type %in% c("violent or sexual", "vehicle crime", "theft from person", "shoplifting", "robbery", "burglary", "bicylce theft")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  st_transform("EPSG:27700")

{r echo=FALSE}

# Wrangle data 

crime_frequency <- london_crimes|>
  # records types of crimes in each location 
  select(type, location_type) |>
  # counts number of crimes in each location 
  count(type, location_type)

london_borough <- read_sf("https://data.london.gov.uk/download/london_boroughs/9502cdec-5df0-46e3-8aa1-2b5c5233a31f/London_Boroughs.gpkg") |>
  select(name, geom) |>
  filter(name %in% c("Merton", "Richmond upon Thames", "Kingston upon Thames", "Wandsworth")) |>
  st_transform("EPSG:27700")

Table of important crimes in different locations

{r echo=FALSE}

# table of crimes that have been shortlisted in the dataset
crime_frequency_wider <- crime_frequency |>
  st_drop_geometry() |>
  pivot_wider(names_from = type, values_from = n) |>
  gt() |>
  fmt_number(columns = where(is.numeric), decimals = 0) |>
  # provides colors for 2 of the crimes below
  data_color(columns = "violent or sexual", palette = "Oranges") |> 
  data_color(columns = "vehicle crime", palette = "Blues") |>
  data_color(columns = "theft from person", palette = "Reds")

crime_frequency_wider

This table presents a list of the important types of crimes, represented as columns along with the different location types of locations that these crimes occur in, which are represented as rows. From the list of the different important crime types, 3 of these crime types have been chosen as the most important crime types that the police should focus on tackling. Each of these 3 different crime types will further be presented through a series of maps and charts, followed by an analysis of each diagram.

Bar plot of vehicle crime

{r echo=FALSE}

# bar plot of violent or sexual crime 
vehicle_crime_bar_plot <- crime_frequency |> 
  drop_na(location_type) |>
  # only focuses on violent or sexual crime 
  filter(type == "vehicle crime") |> 
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(location_type, n))) +
  labs(
    title = "Vehicle Crime data in South West BCU",
    caption = "Data from the UK Government",
    x = "Number of vehicle crimes in each location",
    y = NULL
  ) +
  theme_minimal()

vehicle_crime_bar_plot

This bar plot illustrates the counts of vehicle crime incidents across the South West BCU. Based on the figure shown, the parking area location shows the highest number of vehicle crime incidents, nearing a value of 1500 incidents. In contrast, the race track location shows the least number of vehicle crime incidents, almost nearing a value of 0 incidents. It is evident from these findings, that some places within BCU are more prone to vehicle-related offences than others. To combat the problem successfully, the police should focus on patrolling locations such as parking spaces, supermarkets and sporting grounds.

Bar plot of violent or sexual crime

{r echo=FALSE}

# bar plot of violent or sexual crime 
violent_sexual_crime_bar_plot <- crime_frequency |> 
  drop_na(location_type) |>
  # only focuses on violent or sexual crime 
  filter(type == "violent or sexual") |> 
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(location_type, n))) +
  labs(
    title = "Violent or Sexual Crime data in South West BCU",
    caption = "Data from the UK Government",
    x = "Number of violent or sexual crimes in each location",
    y = NULL
  ) +
  theme_minimal()

violent_sexual_crime_bar_plot

Bar plot of theft

{r echo=FALSE}


# bar plot of theft
theft_bar_plot <- crime_frequency |> 
  drop_na(location_type) |>
  # only focuses on theft
  filter(type == "theft from person") |> 
  ggplot() +
  geom_col(aes(x = n, y = fct_reorder(location_type, n))) +
  labs(
    title = "Thefts in South West BCU",
    caption = "Data from the UK Government",
    x = "Number of thefts in each location",
    y = NULL
  ) +
  theme_minimal()

theft_bar_plot

The bar diagram shows the distribution of theft incidents within the South West BCU, uncovering the counts of thefts across various locations. The highest number of thefts are reported at London Underground stations, nearing 4000 theft incidents, which makes it the location with the highest number of theft incidents. On the counterpart, places such as prisons and airports show the least theft with 0 incidents, making them the locations with the least number of theft incidents. To effectively combat theft and safeguard public assets, the police should concentrate their patrols on supermarkets, shopping malls and theaters.

Maps of each crime type

{r include=FALSE}

# Calculate Gi* statistic ----

vehicle_crime_gi <- london_crimes |>
  st_intersection(london_borough) |>
  filter(type == "vehicle crime") |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_borough) 

{r echo=FALSE, message=FALSE}

# Plot map ----

# vehicle crime map
vehicle_crime_map <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  # Add density for significant cells
  geom_sf(
    aes(fill = kde), 
    data = vehicle_crime_gi, 
    alpha = 0.8,
    colour = NA
  ) +
  # Add ward boundaries
  geom_sf(data = london_borough, colour = "grey70", fill = NA) + 
  scale_fill_distiller(
    breaks = range(pull(vehicle_crime_gi, kde)),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  fixed_plot_aspect() +
  labs(
    title = "Crime hotspots of vehicle crimes",
    subtitle = str_glue(
      "density of vehicle crimes across all location types"
    ),
    caption = str_glue(
      "Government Licence v3.0. Map data from OpenStreetMap."
    ),
    fill = str_wrap("density of vehicle crimes", width = 5)
  ) 

vehicle_crime_map


{r include=FALSE}

# Calculate Gi* statistic ----

violent_or_sexual_crime_gi <- london_crimes |>
  st_intersection(london_borough) |>
  filter(type == "violent or sexual") |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_borough) 

{r echo=FALSE, message=FALSE}

# Plot map ----

# violent or sexual crime map
violent_or_sexual_crime_map <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  # Add density for significant cells
  geom_sf(
    aes(fill = kde), 
    data = violent_or_sexual_crime_gi, 
    alpha = 0.8,
    colour = NA
  ) +
  # Add ward boundaries
  geom_sf(data = london_borough, colour = "grey70", fill = NA) + 
  scale_fill_distiller(
    breaks = range(pull(violent_or_sexual_crime_gi, kde)),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  fixed_plot_aspect() +
  labs(
    title = "Crime hotspots of violent or sexual crimes",
    subtitle = str_glue(
      "density of violent or sexual crimes across all location types"
    ),
    caption = str_glue(
      "Government Licence v3.0. Map data from OpenStreetMap."
    ),
    fill = str_wrap("density of violent or sexual crimes", width = 5)
  ) 

violent_or_sexual_crime_map



{r include=FALSE}

# Calculate Gi* statistic ----

theft_gi <- london_crimes |>
  st_intersection(london_borough) |>
  filter(type == "theft from person") |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_borough) 

{r echo=FALSE, message=FALSE}

# Plot map ----

# theft crime map
theft_map <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  # Add density for significant cells
  geom_sf(
    aes(fill = kde), 
    data = theft_gi, 
    alpha = 0.8,
    colour = NA
  ) +
  # Add ward boundaries
  geom_sf(data = london_borough, colour = "grey70", fill = NA) + 
  scale_fill_distiller(
    breaks = range(pull(theft_gi, kde)),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  fixed_plot_aspect() +
  labs(
    title = "Crime hotspots of thefts",
    subtitle = str_glue(
      "density of thefts across all location types"
    ),
    caption = str_glue(
      "Government Licence v3.0. Map data from OpenStreetMap."
    ),
    fill = str_wrap("density of thefts", width = 5)
  ) 

theft_map

A Gi-star analysis of vehicle crimes, violent or sexual crimes and thefts within South West London boroughs reveals distinct spatial patterns and hotspots.

The vehicle crime map shows high expected crime rates within areas such as Richmond, Putney and Tooting, suggesting a need for strategic patrolling from Richmond to Tooting for enhanced surveillance and crime deterrence. Additionally, high expected violent or sexual crime rates are shown to be happening in similar areas such as Hampton Wick, Wimbledon and Tooting, suggesting an imperative need for focused patrols and collaboration to enhance surveillance. Further to this, thefts are expected to happen largely in Hampton Wick, stressing the need for the patrol team to police these areas effectively.

In this regard, patrol teams should prioritize these identified hotspots, through heightening surveillance, engagement with community members and collaboration with local businesses to enhance security and safety in these regions.
                                                                                                                                                                                                                                                                                                                                                                                                                            Conclusion

In summary, the strategic recommendations outlined in the report strive to tackle the current patterns and locations of different crime types in the South West BCU zone. It is clear from the analysis of vehicle thefts, violence or sex-related crimes and thefts that some specific locations have higher crimes than others, requiring focused police patrol. Through increased surveillance, engagement with the community and cooperation with local businesses, this will enable the police to better handle crime by ensuring that safety and security within the hotspot areas are improved.
