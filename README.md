Timber harvest: https://data.fs.usda.gov/geodata/edw/datasets.php?xmlKeyword=harvest

California Data set: https://www.kaggle.com/datasets/ananthu017/california-wildfire-incidents-20132020/data

CA boundary : https://data.ca.gov/dataset/ca-geographic-boundaries


---
title: "ESP Wildfire Project"
author: "Tarilyn Tong and Charlotte He"
date: "2025-03-09"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## ESP 106 Wildfire Project Presentation

Connection Between Wildfires and Logging Activity in California
Question: Does logging activity affect wildfire occurrences? 
Hypothesis: In areas with higher logging activity, the acres burned from wildfires will increase.

Description / Background:
Wildfires are a natural and essential part of many Californian ecosystems, playing a key role in maintaining balance by supporting new growth and recycling nutrients. However, their continuous impact on human infrastructure, communities, and threats to the ecosystem can be devastating. For our project, we want to explore the connections between wildfires and logging within California’s national forests. 
Specifically, California has been known to have the most wildfires in terms of the number of wildfires and acres burned. As we are impacted directly, these fires are devastating for many residents living near wildfire events, and due to the recent events of the Los Angeles fires, researching the potential factors causing these events is essential for proposing solutions. By investigating the logging activities within the national forests, we should be able to see the relationship between the wildfires and the amount of trees in those areas. 
California wildfires are due to the significant climate change factors of warmer temperatures and reduced precipitation. With less precipitation, vegetation within the national forests will become dry, increasing the possibility of potential ignition. From 2020 to 2023, the annual average number of acres burned was about three times higher than in 2010 (OEHHA 2024). Although wildfires are a natural occurrence and will continue to be part of California’s landscape, logging practices have proceeded to remove most fire-resistant trees for timber, leaving young shrubs and saplings that are more susceptible to ignition, to fight for themselves (CALWILD 2025). Logging also increases fire risk by assembling treetops, needles, leaves, branches/limbs, and other parts of trees that are not collected, on the ground escalating the potential fuel available for the next wildfire (‘Salvage Logging - Sierra Forest Legacy’ 2025, para. 2). Observing and understanding how logging activities influence wildfire spread can help mitigate fire risks while balancing economic and environmental priorities.




In this project, we plan to cover the extent of wildfires within California and their connection to tree-logging activity.

Wildfire Variables:
AcresBurned
Active
AdminUnit - responding unit to deal with fires
ArchiveYear
CalFireIncident - is the incident treated as a CalFire incident
Counties
CountyIDs
Latitude
Longitude
Name- Name of the fire


Reading in CSV and then prepare data for analysis ("data wrangling")
```{r Wildfire Data}
library(tidyr)
library(dplyr)
library(sf)

ca_wildfire = read.csv("/Users/tarilyntong/Desktop/ESP 106/Final Project/California_Fire_Incidents.csv")
logging = st_read("/Users/tarilyntong/Desktop/ESP 106/Final Project/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")

wildfire_summ = ca_wildfire %>%
  group_by(Counties, ArchiveYear) %>%
  summarise(Total_AcresBurned = sum(AcresBurned, na.rm = TRUE), .groups = "drop")
```

Convert wildfire data using "sf" package to get latitude and longitude. (the package is tool for spatial vector data (points, lines, polygons, etc.)) 
```{r}
wildfire_polygons <- wildfire_sf %>%
  filter(ArchiveYear == 2019) %>%
  st_buffer(dist = 5000)

if (st_crs(wildfire_polygons)$units != "m") {
  wildfire_polygons <- st_transform(wildfire_polygons, crs = 32610)
}

```
Convert logging data and find wildfires within and near logging zones
```{r}
library(dplyr)

ca_forests = c("Shasta-Trinity National Forest", "Sierra National Forest", "Los Padres National Forest", "Klamath National Forest", "Sequoia National Forest", "Six Rivers National Forest", "Mendocino National Forest", "Stanislaus National Forest", "Angeles National Forest", "Cleveland National Forest", "Eldorado National Forest", "Inyo National Forest", "Klamath National Forest", "Lassen National Forest", "Modoc National Forest", "Plumas National Forest","Rogue River-Siskiyou National Forest", "San Bernardino National Forest", "Tahoe National Forest")


logging_ca = logging %>%
  filter(ADMIN_FO_1 %in% ca_forests)
logging_ca = logging_ca %>% 
  filter(!st_is_empty(geometry))
logging_ca <- logging %>%
  filter(ADMIN_FO_1 %in% ca_forests) %>%
  filter(FY_COMPLET == 2019)

logging_ca =  st_transform(logging_ca, crs = 4326)
```
CA shape file and changing the CRS to 4326
```{r}
n_cali = st_read("/Users/tarilyntong/Desktop/ESP 106/Final Project/ca_state/CA_State.shp")
n_cali =  st_transform(n_cali, crs = 4326)
st_crs(n_cali) ==st_crs(wildfire_sf)
```
Plot the CA outline and wildfire and logging data
```{r}
library(ggplot2)
library(sf)
library(dplyr)

ggplot() + geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5)+ geom_sf(data = wildfire_polygons, fill = "red", color = "red", alpha = 0.5) + theme_minimal() + labs(title = "Wildfire Incidents in California (2019)")

ggplot() + geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5)+geom_sf(data = logging_ca, color = "blue", alpha = 0.4, size = 0.5) + theme_minimal()+ labs(title = "Logging Activity in California National Forests (2019)")
```

Create a bar plot showing the number of logging activities against the different national forests.
```{r}
logging_summ <- logging_ca %>%
  st_drop_geometry() %>%
  count(ADMIN_FO_1, name = "n")

ggplot(logging_summ, aes(x = reorder(ADMIN_FO_1, n), y = n, fill = ADMIN_FO_1)) + 
  geom_col(show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Logging Activity in California National Forests (2019)", 
       x = "National Forest", 
       y = "Number of Logging Activities") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
Find Overlap between Wildfire and Logging Data 
```{r}
# Fix invalid geometries in both wildfire and logging data
wildfire_polygons <- wildfire_polygons %>%
  st_make_valid()

logging_ca <- logging_ca %>%
  st_make_valid()

overlap_sf <- st_intersection(wildfire_polygons, logging_ca)
```
Create a map with overlap between both data sets
```{r}
ggplot() + 
  geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5) + 
  geom_sf(data = wildfire_polygons, aes(fill = "Wildfire"), color = "red", alpha = 0.5) + geom_sf(data = logging_ca, aes(fill = "Logging"), color = "blue", alpha = 0.7) + 
  geom_sf(data = overlap_sf, aes(fill = "Overlap"), color = "yellow", alpha = 1) + 
  scale_fill_manual(values = c("Wildfire" = "red", "Logging" = "blue", "Overlap" = "yellow")) + theme_minimal() + labs(title = "Wildfire and Logging Activity Overlap in California (2019)", fill = "Activity Type") + theme(legend.position = "right") 
```


correlation plot fixed with tarilyn

```{r}

library(dplyr)
library(sf)
library(terra)
library(lubridate)
library(ggplot2)
library(stars)

logging <- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")  # Replace with actual file path
ca_wildfire <- read.csv("C:/Users/candl/OneDrive/Desktop/esp prject/California_Fire_Incidents.csv")
n_cali<- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/ca_state/CA_State.shp")


fire_sf <- st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326)

st_crs(fire_sf) <- 4326

fire_sf <- st_transform(fire_sf, st_crs(n_cali))
fire_sf <- st_transform(fire_sf, st_crs(logging))
logging_sf <- st_transform(logging, st_crs(n_cali))


fire_logging_overlap <- st_join(fire_sf, logging_sf, left = FALSE, join = st_intersects)




correlation_result <- cor(fire_logging_overlap$AcresBurned,fire_logging_overlap$NBR_UNITS_, use = "complete.obs")
cor.test(fire_logging_overlap$AcresBurned,fire_logging_overlap$NBR_UNITS_, use = "complete.obs")

scatterplot <- ggplot(fire_logging_overlap, aes(x = NBR_UNITS_, y = AcresBurned)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Correlation Between Logging Intensity and Fire Severity",
       x = "Logged Acres",
       y = "Acres Burned") +
  theme_minimal()



```










Create bar plot showing the number of acres burned against different logging areas
```{r}

library(tidyr)
library(dplyr)
library(sf)
library(terra)
library(lubridate)
library(ggplot2)
library(stars)


logging_sf <- st_read("c:/Users/candl/OneDrive/Desktop/esp prject/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")  # Replace with actual file path


# Assuming 'fire_crs' contains your transformed fire data
fires_df <- fire_crs  

# Convert fire data to SF object
fires_sf <- st_as_sf(fires_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform the CRS of fire data to match the logging data's CRS
fires_sf <- st_transform(fires_sf, st_crs(logging_sf))

# Disable s2 geometry library 
sf_use_s2(FALSE)

# Align bounding boxes of fire data and logging data
st_bbox(fires_sf) <- st_bbox(logging_ca)

# Perform spatial join between fire data and logging data (finding where fires intersect logging areas)
fires_with_logging <- st_join(fires_sf, logging_ca, join = st_intersects)

# Summarize total fire size for each logging area
fire_summary <- fires_with_logging %>%
  group_by(ADMIN_FO_1) %>%
  summarise(total_fire_size = sum(AcresBurned, na.rm = TRUE)) %>%
  filter(!is.na(ADMIN_FO_1))%>%
  arrange(desc(total_fire_size))%>%
  filter(!is.na(total_fire_size))

# Create a bar plot showing the total fire size in each logging area
ggplot(fire_summary, aes(x = reorder(ADMIN_FO_1, -total_fire_size), y = total_fire_size)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Total Acres Burned in Different Logging Areas",
       x = "Logging Area",
       y = "Total Acres Burned (Acres)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


```

fixed smooth bar grapgh 


```{r}
library(dplyr)
library(sf)
library(terra)
library(lubridate)
library(ggplot2)
library(stars)





logging <- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")
ca_wildfire <- read.csv("C:/Users/candl/OneDrive/Desktop/esp prject/California_Fire_Incidents.csv")

fires_df <- ca_wildfire 

#convert fire data  to spatial object
fires_sf <- st_as_sf(fires_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform the CRS of fire data to match the logging data's CRS
fires_sf <- st_transform(fires_sf, st_crs(logging))

# Disable s2 geometry library 
sf_use_s2(FALSE)

#Filter Logging Areas in National Forests
loggingca = logging %>%
  filter(grepl("National Forest", ADMIN_FORE, ignore.case = TRUE))

#Remove Empty Geometries
logging_ca = logging[!st_is_empty(logging_sf), ]

#Find Fire Incidents Inside Logging Areas
fire_logging_zones = st_intersection(fires_sf, logging_ca)


#Summarize Fires by National Forest Office (ADMIN_FO_1)
fires_FO  <- fire_logging_zones%>%
  filter(!is.na(ADMIN_FO_1)) %>%
  group_by(ADMIN_FO_1) %>%      
  summarize(AcresBurned = sum(AcresBurned, na.rm = TRUE))


#Sort Logging Areas by Total Fire Size
fire_summary <- fire_logging_zones %>%
  group_by(ADMIN_FO_1) %>%
  summarise(total_fire_size = sum(AcresBurned, na.rm = TRUE)) %>%
  filter(!is.na(ADMIN_FO_1)) %>%
  arrange(desc(total_fire_size)) %>%
  filter(!is.na(total_fire_size))


#plots
ggplot(fire_summary, aes(x = reorder(ADMIN_FO_1, -total_fire_size), y = total_fire_size)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Total Acres Burned in Different Logging Areas",
       x = "Logging Area",
       y = "Total Acres Burned (Acres)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot() +
  geom_sf(data = fire_summary, fill = NA, color = "black") +
  geom_sf(data = fire_summary, aes(color = total_fire_size), alpha = 0.5) +  
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Wildfire Incidents in Logging Areas",
       color = "Acres Burned")


````







