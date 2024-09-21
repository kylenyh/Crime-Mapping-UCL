Introduction

This report will analyze crime data for London's West Area Basic Command Unit (BCU) in 2022 to inform strategic decisions for the police. This will be done so by identifying priority crime types, highlighting wards with the highest incidence rates of vehicle theft and examining the relationship between deprivation and violent/sexual crime rates. This data analysis will be presented in the forms of maps, charts and tables to offer a more comprehensive view of crime patterns on certain areas to aid in effective policing strategies and community engagement initiatives.

{r include=FALSE}

# Load packages

library(ggspatial)  
library(sf)          
library(sfhotspot)  
library(gt)
library(tidyverse)
library(readxl)

{r include=FALSE}

# 1st link

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
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") |>
  st_transform("EPSG:27700")

{r echo=FALSE, message=FALSE}

# 2nd link 

london_density_wards <- read_csv("https://data.london.gov.uk/download/land-area-and-population-density-ward-and-borough/d961f13b-6726-4fa8-823f-03b379429b72/housing-density-ward.csv") |>
  select("Code", "Borough", "Ward_Name", "Year", "Population") |>
  filter(Year == 2022) |>
  filter(Borough %in% c("Ealing", "Hillingdon", "Hounslow"))

{r echo=FALSE}

# 3rd link 

london_wards_file <- tempfile(fileext = ".xlsx")

download.file(
  url = "https://data.london.gov.uk/download/indices-of-deprivation/552e77be-ddb2-416b-a99b-32ead291115b/London%20wards%20id2019%20summary%20measures.xlsx", 
  destfile = london_wards_file, 
  mode = "wb"
)

london_wards_data <- read_excel(london_wards_file) |>
  filter(Borough %in% c("Ealing", "Hillingdon", "Hounslow"))

{r echo=FALSE}

# 4th link 

london_boundary <- tempfile(fileext = ".zip")

download.file(
  url = "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/08d31995-dd27-423c-a987-57fe8e952990/London-wards-2018.zip",
  destfile = london_boundary
)

london_boundary_directory <- str_glue("{tempdir()}/London-wards-2018")

unzip(london_boundary, exdir = london_boundary_directory)

london_wards <- london_boundary_directory |>
  str_glue("/London-wards-2018_ESRI/London_Ward.shp") |>
  read_sf() |>
  filter(DISTRICT %in% c( "Hillingdon", "Ealing", "Hounslow"))

{r echo=FALSE}

# wrangle data 

london_bcu_data <- london_crimes|>
  st_join(london_wards, by = c("geometry" = "geometry")) |>
  filter(DISTRICT %in% c("Hillingdon", "Ealing", "Hounslow"))

{r echo=FALSE}

# counts of every crime type

crime_frequency <- london_bcu_data|>
  # counts number of crimes in each location 
  count(type)

Table of all crimes

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
  data_color(columns = "criminal damage or arson", palette = "Reds")

crime_frequency_wider

Bar plot of all crimes

{r echo=FALSE}

# bar plot of violent or sexual crime 
vehicle_crime_bar_plot <- crime_frequency |> 
  ggplot() +
  geom_col(aes(x = n, y = type)) +
  labs(
    title = "Crime data in West Area BCU",
    caption = "Data from the UK Government",
    x = "Crimes across West Area BCU",
    y = NULL
  ) +
  theme_minimal()

vehicle_crime_bar_plot

Which types of crime should the tasking team be focused on?

The crime table shows a breakdown of recorded incidents across various crime types. This data is further supplemented by a bar chart that accompanies it by providing a visual representation of every crime type that is numerically comprehensive. The bar chart as shown above results in a conclusion that the police should direct their attention towards violent or sexual crime and vehicle crimes due to the high occurrence of recorded crime incidents. In addition, criminal damage to property or arson should be looked at and considered by the police as well, due to the long-term negative effects that it could pose such as property destruction, financial loss and danger to neighbourhoods.

Map of important crimes

{r include=FALSE}

# calculates Gi* statistic

vehicle_crime_gi <- london_crimes |>
  filter(type == "vehicle crime") |>
  st_intersection(london_wards) |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_wards) 

{r echo=FALSE, message=FALSE}


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
  geom_sf(data = london_wards, colour = "grey70", fill = NA) + 
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
    fill = str_wrap("concentration of vehicle crimes", width = 5)
  ) 

vehicle_crime_map

{r include=FALSE}

# calculates Gi* statistic

violent_or_sexual_crime_gi <- london_crimes |>
  st_intersection(london_wards) |>
  filter(type == "violent or sexual") |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_wards) 

{r echo=FALSE, message=FALSE}


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
  geom_sf(data = london_wards, colour = "grey70", fill = NA) + 
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
    fill = str_wrap("concentration of violent or sexual crimes", width = 5)
  ) 

violent_or_sexual_crime_map

{r include=FALSE}

# calculates Gi* statistic

criminal_damage_gi <- london_crimes |>
  st_intersection(london_wards) |>
  filter(type == "criminal damage or arson") |>
  hotspot_gistar(bandwidth_adjust = 0.25) |>
  filter(gistar > 0, pvalue < 0.05) |>
  st_intersection(london_wards)

{r echo=FALSE, message=FALSE}


# theft crime map
criminal_damage_map <- ggplot() + 
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  # Add density for significant cells
  geom_sf(
    aes(fill = kde), 
    data = criminal_damage_gi, 
    alpha = 0.8,
    colour = NA
  ) +
  # Add ward boundaries
  geom_sf(data = london_wards, colour = "grey70", fill = NA) + 
  scale_fill_distiller(
    breaks = range(pull(criminal_damage_gi, kde)),
    labels = c("lower", "higher"),
    direction = 1
  ) +
  fixed_plot_aspect() +
  labs(
    title = "Crime hotspots of criminal damage or arson",
    subtitle = str_glue(
      "density of thefts across all location types"
    ),
    caption = str_glue(
      "Government Licence v3.0. Map data from OpenStreetMap."
    ),
    fill = str_wrap("concentration of criminal damage or arson", width = 5)
  ) 


criminal_damage_map

Which parts of the BCU should the team focus on patrolling?

A Gi-star analysis of vehicle crimes, violent or sexual crimes and crminal damage or arson crimes within the West Area BCU of London reveals distinct spatial patterns and hotspots.

Across all 3 maps that have been provided of each of the 3 crime types, areas such as Hounslow, Uxbridge, Chriswich, show the highest expected crime rates. In this regard, it would be best for the police patrol team to prioritize patrolling these identified hotspots, through heightened surveillance, engagement with community members and collaboration with local businesses to enhance security and safety in these areas.

Incidence rate map of vehicle theft across wards

{r echo=FALSE, message=FALSE}

# joins vehicle crime data with wards
vehicle_crimes_data <- london_wards |>
  st_join(london_bcu_data, by = c("geometry" = "geometry")) |>
  filter(type == "vehicle crime") |>
  count(NAME.x)


# calculates incidence rate of vehicle theft rate in each ward
london_vehicle_theft <- vehicle_crimes_data |>  
  left_join(london_density_wards, by = c("NAME.x" = "Ward_Name")) |>
  mutate(vehicle_crime_rate = n / (Population/100))

# gets the top five wards which have the highest vehicle theft rates
top_five <- london_vehicle_theft |>
  slice_max(vehicle_crime_rate, n = 5)

{r echo=FALSE}

# chloropleth map of incidence rate of vehicle theft rate in each ward
vehicle_chloro_map <- ggplot() +
  # Adds the base map image
  annotation_map_tile(type = 'cartolight', progress = "none") +
  # Adds the fill layer with the different incident rates
  geom_sf(aes(fill= vehicle_crime_rate), data = london_vehicle_theft, alpha = 0.8) +
  geom_sf_label(
    aes(label = str_wrap(NAME.x, 10)),
    data = top_five,
    # Make labels semi-transparent
    alpha = 0.5,
    # Set label colour
    colour = "seagreen",
    # Reduce space between lines on labels with multiple lines
    lineheight = 1,
    # Reduce label font size
    size = 2.5,
    # Reduce padding around labels
    label.padding = unit(0.1, "lines"),
    # Remove border around labels
    label.size = NA
  ) +
  # Adds a title, subtitle etc
  labs(
    fill = str_wrap("Number of incidents per 100 people", width = 5),
    title = "Ward - Level Distribution of Vehicle Crime",
    subtitle = "West BCU area"
  ) +
  # Adds the colour palette
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_void() +
  theme(
    # Customises Aesthetics
    plot.title = element_text(size = rel(1.4)),
    plot.subtitle = element_text(size = rel(1.2)),
    plot.caption = element_text(colour = 'grey44', hjust = 0)
  )
theme(panel.border = element_rect(colour = "black", fill = NA))

vehicle_chloro_map


Which wards have the highest incidence rates of vehicle theft, based on residential population?

The provided map above highlights wards with varying rates of incidents of vehicle crime within the West Area BCU, with darker shades representing higher frequencies. Heathrow Villages, with the darkest color, indicates the highest rate of vehicle thefts, suggesting a critical need for the potential deployment of the local tasking team.

In contrast, surrounding wards such as South Ruislip and Chiswick Homefields, exhibit lighter shades suggesting lower rates of vehicle thefts. This could be potentially due to the efficacy of current preventative measures or differing patterns of vehicle ownership and usage. The geographic disparity in vehicle crime rates across the wards suggests that targeted interventions are essential.

Scatter plot of deprivation and violent or sexual crime data

{r echo=FALSE}


# joins violent or sexual crime data with wards
violent_crimes_data <- london_wards |>
  st_join(london_bcu_data, by = c("geometry" = "geometry")) |>
  filter(type == "violent or sexual") |>
  count(NAME.x)


# calculates incidence rate of violent and sexual crime rate in each ward
london_violent_crime <- violent_crimes_data |>  
  left_join(london_wards_data, by = c("NAME.x" = "Ward Name")) |>
  mutate(violent_crime_rate = n / (Population/100))

# joins london violent crime with deprivation data of each ward
deprivation_data <- london_violent_crime |>
  left_join(london_wards_data, by = c("NAME.x" = "Ward Name"))

{r echo=FALSE}


# scatter plot between deprivation and the incidence rate of violent and sexual crime 
ggplot(deprivation_data, aes(x = `Rank of worst LSOA.x`, y = violent_crime_rate)) +
  geom_vline(
    xintercept = median(pull(deprivation_data, `Rank of worst LSOA.x`)),
    linetype = "22"
  ) +
  geom_hline(
    yintercept = median(pull(deprivation_data, violent_crime_rate)),
    linetype = "22"
  ) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "grey20") +
  geom_point(alpha = 0.5) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Relationship between deprivation and incidence rate of violent or sexual crime",
    subtitle = str_glue(
      "West BCU in London"
    ),
    x = "Worst rankings of LSOAs",
    y = "Incidence rate of violent/sexual crime rates per 100 people"
  ) +
  theme_minimal()

What is the relationship between deprivation and the incidence rate of violent and sexual crime in LSOAs in the West Area BCU?

The scatter plot shows the West Area BCU's challenge with violent and sexual crimes across varying levels of deprivation among its LSOAs. The visual distribution indicates a discernible trend where higher deprivation scores do not always align with increased crime rates, challenging preconceived notions of a direct, positive correlation between deprivation and crime incidence.

A closer view into the plot illustrates that even the poorest areas indicate some moderate levels of crime, whereas some in the affluent areas show a higher level of crime. This could be evidence of the involvement of other factors or programs with proven success in some deprived areas. The downward trending line of the best fit, although exhibiting considerable scatter around the line, suggests that deprivation by itself may not be the only factor of violent and sexual crimes.

This piece of knowledge is important for the chief superintendent. It implies that although deprivation is a factor of crime, there are nuances that a one-dimensional approach to policing and crime prevention cannot address. This suggests that strategies developed in response to this crime should be multidimensional, drawing not only upon socio-economic data but also other community-specific indicators that might influence crime rates.

What are the limitations of the crime data that the conclusions are based on?

Firstly, the 2022 crime data largely relies on incidents reported to the police. This collected data could lead to underestimations and inaccuracies since not all victims will report it due to due to the sensitive nature of these crimes, distrust in authorities, or fear of stigma. Consequently, these crimes may be greater than what the records indicate.

Secondly, this data captures a single year and might not reflect long-term trends. An anomalous spike or dip in crime for 2022 could skew perceptions and strategic recommendations. Crime is dynamic; hence, temporal analyses that incorporate several years of data can provide a more robust foundation for understanding trends.

Thirdly, the LSOA data might conceal hyper-local variations. Crimes can be highly localized, often concentrated around specific streets or even buildings, which the LSOA-level data cannot capture.

Moreover, the data analysis presumes a direct correlation between deprivation and crime rates, which is an oversimplification. The causality of crime is complex, interwoven with a multitude of socio-economic factors, community resources, policing efforts and urban environments that the data might not fully encompass.

Additionally, although the deprivation rankings are valuable, they may not be up-to-date hence failing to reflect on recent socio-economic changes. These can encompass a variety of factors, not all of which may be relevant to crime.

Furthermore, the maps and scatter plot provided offer visual representations that guide strategic decisions, but they are not solely the best resource for a strategic, multi-faceted approach. The data instead is a starting point for a more in-depth, qualitative assessment that should include community engagement and the consideration of patrol police local teams in certain areas.
