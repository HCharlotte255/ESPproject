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
Convert wildfire and logging data using "sf" package to get latitude and longitude. (the package is tool for spatial vector data (points, lines, polygons, etc.)) 

```{r}
wildfire_sf = st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
logging_sf = st_as_sf(logging, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

```
Convert logging data and find wildfires within logging zones 

```{r}


```

