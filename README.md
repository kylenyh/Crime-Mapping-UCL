# Crime-Mapping

This module will equip students with the theoretical and practical knowledge of crime mapping. It will cover principles that are specific to the analysis of geographical information and provide students with foundational Geographic Information System (GIS) skills. Particular attention will be paid to the strengths and weaknesses of different types of mapping techniques and cartographic skills. At the end of the module students should be able to competently generate a range of crime maps and understand the benefits and limitations of doing so.

Teaching time for this module is primarily devoted to hands-on practical computer workshops. There is an expectation that students will prepare for these workshops in advance by engaging with the teaching materials. Weekly Moodle quizzes contribute to half of the marks for this module, with coursework making up the remaining marks.

# Key Projects

#### Vehicle Theft Density  Map
* Generated a density map illustrating thefts from vehicles in Vancouver throughout 2020, using crime data from a CSV file
* Integrated a base map and overlaid Vancouver's neighborhood boundaries and names for clear contextualization
* Visually represented the spatial distribution of thefts, aiding in hotspot identification and informing crime prevention strategies

**Libraries**: `ggspatial`, `sf`, `sfhotspot`, `tidyverse`

#### Second Vehicle Theft Density Map
* Created a density map depicting two-wheeled vehicle theft prevalence in Prague during 2022
* Used KDE to identify statistically significant hotspots, pinpointing areas of heightened criminal activity
* Ensured accuracy by limiting crime data to Prague's city boundary, offering insights crucial for law enforcement and urban planning strategies

**Libraries**: `ggspatial`, `sf`, `sfhotspot`, `tidyverse`

#### Fatal Shootings Density Map
* Generated a density map illustrating fatal shootings in New York City throughout 2019, based on crime data from a CSV file
* Incorporated NYPD precinct boundaries and overlaid precinct numbers on the map for enhanced spatial context
* Visualized the concentration of fatal incidents to inform law enforcement strategies and community engagement efforts aimed at addressing gun violence
  
**Libraries**: `ggspatial`, `sf`, `sfhotspot`, `tidyverse`

#### Stalking Incidence Rate Map
* Created a choropleth map showing stalking incident rates across Queensland police divisions in 2018, integrating incident counts and population data
* Provided insights into the relative prevalence of stalking crimes, highlighting areas of heightened concern
* Identified and displayed the names of the top 10 divisions with the highest stalking rates to guide policy-making and resource allocation

**Libraries**: `leaflet`, `sf`, `readxl`, `tidyverse`

#### Quarto Report of Crimes Types 
* Created a detailed report in R analyzing specific crime types within the South West BCU of London
* Utilized tables, maps, and charts to visually represent key crime types and high-risk areas
* Provided data-driven recommendations for tasking team deployment, focusing on prioritizing crime problems and patrol areas based on current trends and geographic hotspots in the South West BCU

**Libraries**: `ggspatial`, `sf`, `sfhotspot`, `gt`, `tidyverse`

#### Second Quarto Report of Crimes Types
* Created a Quarto report in R to analyze 2022 crime data for the West Area Basic Command Unit (BCU)
* Identified priority crime types and hotspots for the Local Tasking Team, focusing on vehicle theft incidence rates by ward
* Examined the relationship between deprivation levels and rates of violent and sexual crimes to inform potential government funding applications, while acknowledging limitations of the data

**Libraries**: `ggspatial`, `sf`, `sfhotspot`, `gt`, `tidyverse`

