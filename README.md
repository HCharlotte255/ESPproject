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

In this project, we plan to cover the extent of wildfires within California and its connection to tree logging activity.

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
Name- name of the fire

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
wildfire_sf <- wildfire_sf %>%
  filter(st_coordinates(.)[,2] >= 32 & st_coordinates(.)[,2] <= 42, st_coordinates(.)[,1] >= -124 & st_coordinates(.)[,1] <= -114)

```
Convert logging data and find wildfires within and near logging zones

```{r}
library(dplyr)

ca_forests = c("Shasta-Trinity National Forest", "Sierra National Forest", "Los Padres National Forest", "Klamath National Forest", "Sequoia National Forest", "Six Rivers National Forest", "Mendocino National Forest", "Stanislaus National Forest", "Angeles National Forest", "Cleveland National Forest", "Eldorado National Forest", "Inyo National Forest", "Klamath National Forest", "Lassen National Forest", "Modoc National Forest", "Plumas National Forest","Rogue River-Siskiyou National Forest", "San Bernardino National Forest", "Tahoe National Forest")


logging_ca = logging %>%
  filter(ADMIN_FO_1 %in% ca_forests)
logging_ca = logging_ca %>% 
  filter(!st_is_empty(geometry))

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

ggplot() + geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5)+ geom_sf(data = wildfire_sf, color ="red", alpha = 0.4, size = 0.5) + geom_sf(data = logging_ca, color = "blue", alpha = 0.4, size = 0.5) + theme_minimal()+ labs(title = "Wildfire and Logging Activity in California")



```

Create a bar plot data frame
```{r}
wildfire_sf = st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
logging_sf = st_as_sf(logging, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

ca_forests = c("Shasta-Trinity National Forest", "Sierra National Forest", "Los Padres National Forest", "Klamath National Forest", "Sequoia National Forest", "Six Rivers National Forest", "Mendocino National Forest", "Stanislaus National Forest", "Angeles National Forest", "Cleveland National Forest", "Eldorado National Forest", " Inyo National Forest", "Klamath National Forest", "Lassen National Forest", "Modoc National Forest", "Plumas National Forest","Rogue River-Siskiyou National Forest", "San Bernardino National Forest", "Tahoe National Forest")

loggingca = logging %>%
  filter(grepl("National Forest", ADMIN_FORE, ignore.case = TRUE))

logging_ca = logging[!st_is_empty(logging), ]

fire_logging_zones = st_intersection(wildfire_sf, logging_ca)



# checks and transforms the 
n_Cali = st_read("C:/Users/candl/OneDrive/Desktop/esp prject/ca_state/CA_State.shp")
Cali=  st_transform(n_Cali,4269)

st_crs(Cali)


#data frame 
#transform csv to spatial format 
wildfire_sf = st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

#transform  fire data to match logging data
fire_crs<- st_transform(wildfire_sf, st_crs(logging_sf))

#perform spatial joins (assign fires to logging areas)
fires_w_logging <- st_join(fire_crs,logging, join= st_intersects)
print(fires_w_logging)

#7
fire_summ <- fires_w_logging %>%
  filter(!is.na(AdminUnit)) %>%  
  select(AdminUnit, AcresBurned)  


Fire_tgt  <- fires_w_logging %>%
  filter(!is.na(AdminUnit)) %>%
  group_by(AdminUnit) %>%      
  summarize(total_fire_size = sum(AcresBurned, na.rm = TRUE))


fire_summ$geometry <- NULL

firegrouped <- aggregate(AcresBurn ~ AdminUnit, data = fire_summ, FUN = sum, na.rm = TRUE)
firegroup<- fire_summ[-c(6,7),]

firegroup <- firegroup %>%
  mutate(AdminUnit = trimws(AdminUnit))

firegroup  <- firegroup %>%
  filter(!is.na(AdminUnit)) %>%
  group_by(AdminUnit) %>%      
  summarize(total_AcresBurn = sum(total_fireburn, na.rm = TRUE))

firegroup <- firegroup %>%
  mutate(AdminUnit = recode(AdminUnit, 
                            "Angeles National Forest / Los Angeles County Fire Department" = "Angeles National Forest",
                            "Angeles National Forest and LA County Fire Department" = "Angeles National Forest",
                            "Angeles National Forest/Los Angeles County Fire" = "Angeles National Forest",
                            "BLM Northern District (NOD)" = "BLM Northern California District",
                            "Bureau of Indian Affairs - CAL FIRE / Riverside County Fire" = "Bureau of Indian Affairs",
                            "Bureau of Land Management - Northern CA District" = "Bureau of Land Management",
                            "Bureau of Land Management - Owens Valley District" = "Bureau of Land Management",
                            "Bureau of Land Management - Northern California District" = "Bureau of Land Management",
                            "Bureau of Land Management Northern California District" = "Bureau of Land Management",
                            "Bureau of Land Mangement" = "Bureau of Land Management",
                            "CAL FIRE / San Diego County Fire Authority" = "CAL FIRE / San Diego County Fire",
                            "CAL FIRE / USFS - El Dorado National Forest" = "El Dorado National Forest",
                            "CAL FIRE Amador - El Dorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador El-Dorado" = "El Dorado National Forest",
                            "CAL FIRE Amador El-Dorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador, El Dorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador- El Dorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador- El Dorado Unit / Sac Metro Fire" = "El Dorado National Forest",
                            "CAL FIRE Amador- El Dorado Unit / USFS El Dorado National Forest / Lake Valley Fire" = "El Dorado National Forest",
                            "CAL FIRE Amador-El Dorado" = "El Dorado National Forest",
                            "CAL FIRE Amador-El Dorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador-Eldorado Unit" = "El Dorado National Forest",
                            "CAL FIRE Amador El Dorado Unit" = "El Dorado National Forest",))
firegroup  <- firegroup %>%
  filter(!is.na(AdminUnit)) %>%
  group_by(AdminUnit) %>%      
  summarize(AcresBurn = sum(total_AcresBurn, na.rm = TRUE))



str(firegroup)  # Check data types
sum(is.na(firegroup$AcresBurn))  # Check for missing values
sum(is.na(firegroup$AdminUnit))  # Check for missing categories
unique(firegroup$AdminUnit) 
  

ggplot(fire_summ, aes(x = AdminUnit, y = AcresBurn)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Fire Size in Different Logging Areas (CA Only)",
       x = "Logging Area",
       y = "Fire Size (Acres)") 
```

FIXED CODE 
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

3/16, 4:33, bar graph and data summary 


```{r}
logging_sf <- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")
fire_crs <- read.csv("C:/Users/candl/OneDrive/Desktop/esp prject/California_Fire_Incidents.csv")

fires_df <- fire_crs  

#convert fire data  to spatial object
fires_sf <- st_as_sf(fires_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform the CRS of fire data to match the logging data's CRS
fires_sf <- st_transform(fires_sf, st_crs(logging_sf))

# Disable s2 geometry library 
sf_use_s2(FALSE)

#Filter Logging Areas in National Forests
loggingca = logging_sf %>%
  filter(grepl("National Forest", ADMIN_FORE, ignore.case = TRUE))

#Remove Empty Geometries
logging_ca = logging_sf[!st_is_empty(logging_sf), ]

#Find Fire Incidents Inside Logging Areas
fire_logging_zones = st_intersection(fires_sf, logging_ca)

-------------
TROUBLE SHOOT IF NEEDED
#Align Fire and Logging Data’s Bounding Boxes (Fix Display Issues)
st_bbox(fires_sf) <- st_bbox(logging_ca)

#Reproject Data to a UTM Coordinate System
projected_crs <- 32610  
logging_sf_proj <- st_transform(logging_sf, crs = projected_crs)
fire_sf_proj <- st_transform(fire_sf, crs = projected_crs)
fires_in_logging <- st_intersection(fire_sf_proj, logging_sf_proj)
------

Summarize Fires by National Forest Office (ADMIN_FO_1)
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







