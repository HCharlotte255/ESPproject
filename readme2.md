---
title: "ESP Wildfire Project"
author: "Tarilyn Tong and Charlotte He"
date: "2025-03-17"
output:
  html_document:
    df_print: paged
  pdf_document: default
---
```{r}
install.packages("httr", repos = "https://cran.rstudio.com")
library(sf)
library(utils)
library(httr)

logging_zip_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/Actv_TimberHarvest.zip"

# Specify the location to save the zip file
logging_zip_file <- "Actv_TimberHarvest.zip"
logging_unzip_dir <- "S_USA_Actv_TimberHarvest"

# Download the zip file if it doesn't already exist and unzip it
if (!file.exists(logging_zip_file)) {
  download.file(logging_zip_url, logging_zip_file, mode = "wb")
}
if (!dir.exists(logging_unzip_dir)) {
  dir.create(logging_unzip_dir)
}

unzip(logging_zip_file, exdir = logging_unzip_dir)

# Get the .shp file path 
logging_shp_file <- list.files(logging_unzip_dir, pattern = "\\.shp$", full.names = TRUE)[1]

#CA Wildfire dataset
# Define the download URL
url <- "https://www.kaggle.com/api/v1/datasets/download/ananthu017/california-wildfire-incidents-20132020?datasetVersionNumber=1"

# Set the destination file
destfile <- "wildfire_data.zip"

# Perform the download
GET(url, write_disk(destfile, overwrite = TRUE))

# Unzip the file
unzip(destfile, exdir = "wildfire_data")

# List the files in the extracted folder
list.files("wildfire_data")

```
**Description / Background:**

Wildfires are a natural and essential part of many Californian ecosystems, playing a key role in maintaining balance by supporting new growth and recycling nutrients. However, their continuous impact on human infrastructure, communities, and threats to the ecosystem can be devastating. For our project, we want to explore the connections between wildfires and logging within California’s national forests.

Specifically, California has been known to have the most wildfires in terms of the number of wildfires and acres burned. As we are impacted directly, these fires are devastating for many residents living near wildfire events, and due to the recent events of the Los Angeles fires, researching the potential factors causing these events is essential for proposing solutions. By investigating the logging activities within the national forests, we should be able to see the relationship between the wildfires and the amount of trees in those areas. 

California wildfires are due to the significant climate change factors of warmer temperatures and reduced precipitation. With less precipitation, vegetation within the national forests will become dry, increasing the possibility of potential ignition. From 2020 to 2023, the annual average number of acres burned was about three times higher than in 2010 (OEHHA 2024). Although wildfires are a natural occurrence and will continue to be part of California’s landscape, logging practices have proceeded to remove most fire-resistant trees for timber, leaving young shrubs and saplings that are more susceptible to ignition, to fight for themselves (CALWILD 2025). Logging also increases fire risk by assembling treetops, needles, leaves, branches/limbs, and other parts of trees that are not collected, on the ground escalating the potential fuel available for the next wildfire (‘Salvage Logging - Sierra Forest Legacy’ 2025, para. 2). Observing and understanding how logging activities influence wildfire spread can help mitigate fire risks while balancing economic and environmental priorities.

**Description of the data set and exploratory data analysis:** 

In this project, we used two data sets, one from the USDA Forest Service and the other from the California Wildfire Service. The USDA Forest Service’s dataset contains over 200 years worth of data on timber harvests within national forests in the United States.  The USDA Timber Harvest contained information about the logging activity within national forests such as the number of acres, logging technique, and land suitability. The California Wildfire Service data set on the other hand, contains data and information from 2013 to 2020, including the location of wildfires, county names, latitude/longitude values, and the number of acres burned. As our research focuses on California’s National Forests, we must filter and refine the datasets to ensure that our analysis accurately reflects the environment of these forests.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load Required Libraries
```{r}
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)
library(terra)
library(stars)

## Read in Data
```{r}
ca_wildfire <- read.csv("/Users/tarilyntong/Desktop/ESP 106/Final Project/California_Fire_Incidents.csv")
logging <- st_read("/Users/tarilyntong/Desktop/ESP 106/Final Project/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")
n_cali <- st_read("/Users/tarilyntong/Desktop/ESP 106/Final Project/ca_state/CA_State.shp")
```
The Timber Harvest data set consists of 70 variables describing different factors of forest characteristics and the activities that take place within.  In order to explore and utilize the data effectively, research on the different variables was needed so that when they were used within the code, we would reference the correct column. After reviewing the data set, we identified key variables that were relevant for our analysis. Specifically, we wanted to focus on the variables that were related to the location of the logging practices and the year in which the logging practice was completed. Similarly, for the wildfire data set, we focused on the variables that were related to the number of acres burned, location of the fire within California's national forests, and the year in which it occurred. By aligning these variables, we aim to examine potential relationships between timber harvesting and wildfire susceptibility.

## Prepare Wildfire Data
```{r}
wildfire_summ = ca_wildfire %>%
  group_by(Counties, ArchiveYear) %>%
  summarise(Total_AcresBurned = sum(AcresBurned, na.rm = TRUE), .groups = "drop")
```

##Convert wildfire data using "sf" package to get latitude and longitude. (the package is tool for spatial vector data (points, lines, polygons, etc.)) 
```{r}
wildfire_sf = st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
logging_sf = st_as_sf(logging, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Reproject the data to EPSG:3857 (Web Mercator) for better buffering
wildfire_sf_proj <- st_transform(wildfire_sf, crs = 3857)

wildfire_polygons <- wildfire_sf_proj %>%
  filter(ArchiveYear == 2019) %>%
  st_buffer(dist = 500)

wildfire_polygons <- st_transform(wildfire_polygons, crs = st_crs(wildfire_sf))

```

## Filter Logging Data
```{r}
ca_forests <- c("Shasta-Trinity National Forest", "Sierra National Forest", "Los Padres National Forest", 
                "Klamath National Forest", "Sequoia National Forest", "Six Rivers National Forest", 
                "Mendocino National Forest", "Stanislaus National Forest", "Angeles National Forest", 
                "Cleveland National Forest", "Eldorado National Forest", "Inyo National Forest", 
                "Lassen National Forest", "Modoc National Forest", "Plumas National Forest", 
                "San Bernardino National Forest", "Tahoe National Forest")

logging_ca <- logging %>%
  filter(ADMIN_FO_1 %in% ca_forests, FY_COMPLET == 2019) %>%
  filter(!st_is_empty(geometry))
```

## Check CRS Consistency
```{r}
logging_ca =  st_transform(logging_ca, crs = 4326)
n_cali <- st_transform(n_cali, crs = 4326)
st_crs(n_cali) == st_crs(wildfire_sf)
```

## Plot Wildfire and Logging Data
```{r}
library(ggplot2)
library(sf)
library(dplyr)

#Figure 1
ggplot() + geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5)+geom_sf(data = logging_ca, color = "blue", alpha = 0.4, size = 0.5) + theme_minimal()+ labs(title = "Logging Activity in California National Forests (2019)")

#Figure 2
ggplot() + geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5) +geom_sf(data = wildfire_polygons, fill = "red", color = "red", alpha = 0.3, size = 0.5) + coord_sf(xlim = c(-124, -114), ylim = c(32, 42)) + theme_minimal() + labs(title = "Wildfire Incidents in California (2019)")

```
To understand the information provided by these data sets, we created two separate maps illustrating California containing logging activity (Figure 1) and wildfires (Figure 2). Figure 1 illustrates the spatial distribution of logging activity throughout California in 2019, where the blue polygons represent the areas where logging occurred. The distribution of logging activity significantly varies between forests as some have experienced minimal timber harvests while others faced substantial amounts of logging activities (Figure 3). The wildfire data contains information on the increasing number of acres burned over the course of about 200 years. Initial findings suggest that with the increased number of logging practices within the national forests, the more susceptible the land will be to wildfires
.
## Summarize Logging Activities
```{r}
logging_summ <- logging_ca %>%
  st_drop_geometry() %>%
  count(ADMIN_FO_1, name = "n")

#Figure 3
ggplot(logging_summ, aes(x = reorder(ADMIN_FO_1, n), y = n, fill = ADMIN_FO_1)) + 
  geom_col(show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Logging Activity in California National Forests (2019)", 
       x = "National Forest", 
       y = "Number of Logging Activities") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
## Find Overlap Between Wildfires and Logging Areas
```{r}
wildfire_polygons <- st_make_valid(wildfire_polygons)
logging_ca <- st_make_valid(logging_ca)



overlap_sf <- st_intersection(wildfire_polygons, logging_ca)
```

## Plot Overlap Between Wildfire and Logging
```{r}
library(sf)
# Figure 4: Overlap of Wildfire and Logging Activity in California
ggplot() + 
  geom_sf(data = n_cali, fill = "lightgray", color = "black", alpha = 0.5) +
  geom_sf(data = wildfire_polygons, aes(fill = "Wildfire"), color = "red", alpha = 0.5) + 
  coord_sf(xlim = c(-124, -114), ylim = c(32, 42)) + 
  geom_sf(data = logging_ca, aes(fill = "Logging"), color = "blue", alpha = 0.7) +
  geom_sf(data = overlap_sf, aes(fill = "Overlap"), color = "yellow", alpha = 1) +
  scale_fill_manual(values = c("Wildfire" = "red", "Logging" = "blue", "Overlap" = "yellow")) +
  theme_minimal() +
  labs(title = "Wildfire and Logging Activity Overlap in California (2019)", fill = "Activity Type") +
  theme(legend.position = "right")

```

Mapping logging regions and wildfire incidents provides a clear way to identify areas with activity, but our goal was to determine whether logging had any impact on wildfire occurrence. To analyze this, we used the merged data set to examine spatial areas that had experienced both logging and wildfires during the same period. Figure 4 presents a visual representation of the overlap between logging/timber harvest activity and wildfire incidents. This overlap is particularly noticeable in Northern California, near the state lines bordering Northwestern Nevada and Southern Oregon. To highlight this, it is marked with a yellow dot.


## Summarize Fire Incidents in Logging Areas
```{r}
fire_df <- ca_wildfire

fires_sf<-st_as_sf(fire_df,coords = c("Longitude", "Latitude"), crs = 4326)


fires_sf <- st_transform(fires_sf, st_crs(logging))

sf_use_s2(FALSE)
------------------------
loggingca = logging %>%
  filter(grepl("National Forest", ADMIN_FORE, ignore.case = TRUE))

logging_ca = logging[!st_is_empty(logging), ]

fire_logging_zones = st_intersection(fires_sf, logging_ca)

projected_crs <- 32610  
logging_sf_proj <- st_transform(logging, crs = projected_crs)
fire_sf_proj <- st_transform(fires_sf, crs = projected_crs)
fires_in_logging <- st_intersection(fire_sf_proj, logging_sf_proj)


fires_in_logging  <- fires_in_logging%>%
  filter(!is.na(AdminUnit,)) %>%
  group_by(AdminUnit) %>%      
  summarize(AcresBurned = sum(AcresBurned, na.rm = TRUE))



fire_summary <- fire_logging_zones %>%
  group_by(ADMIN_FO_1) %>%
  summarise(total_fire_size = sum(AcresBurned, na.rm = TRUE)) %>%
  filter(!is.na(ADMIN_FO_1))%>%
  arrange(desc(total_fire_size))%>%
  filter(!is.na(total_fire_size))




barplot<- ggplot(fire_summary, aes(x = reorder(ADMIN_FO_1, -total_fire_size), y = total_fire_size)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Total Acres Burned in Different Logging Areas",
       x = "Logging Area",
       y = "Total Acres Burned (Acres)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

```
The bar graph visually represents the extent of wildfire damage by showing the total number of acres burned in different logged areas. The data is arranged in descending order to make patterns easier to identify. The vertical (y-axis) represents the total acres burned, while the horizontal (x-axis) represents the logged areas within California’s national forests. From this, we can clearly see that some regions were affected more than others. Notably, Plumas National Forest experienced the highest number of acres burned in logged areas, whereas Modoc National Forest had the lowest. This plot highlights how wildfire severity varies across different regions, even among forests in close proximity within Northern California.

```{r}
#Ensures coordinates are in same format
fire_sf <- st_as_sf(ca_wildfire, coords = c("Longitude", "Latitude"), crs = 4326)
logging_sf <- st_transform(logging, st_crs(n_cali))

fire_sf <- st_make_valid(fire_sf)
logging_sf <- st_make_valid(logging_sf)


#Create another overlap between fire and logging  
fire_logging_overlap <- st_join(fire_sf, logging_sf, left = FALSE, join = st_intersects)


st_crs(fire_sf)
st_crs(logging_sf)



correlation_result <- cor(fire_logging_overlap$AcresBurned,fire_logging_overlap$NBR_UNITS_, use = "complete.obs")
cor.test(fire_logging_overlap$AcresBurned,fire_logging_overlap$NBR_UNITS_, use = "complete.obs")

scatterplot <- ggplot(fire_logging_overlap, aes(x = NBR_UNITS_, y = AcresBurned)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Correlation Between Logging Intensity and Fire Severity",
       x = "Logged Acres",
       y = "Acres Burned") +
  theme_minimal()

print(scatterplot)
print(correlation_result)
```
A correlation plot helps us determine whether two variables are related and visually represents the strength of their correlation. An upward trend typically indicates a positive relationship, while a downward trend suggests a negative correlation. If the data points form a straight line with no clear trend, it implies little to no relationship between the variables. While our initial findings suggested a connection between logging intensity and wildfire severity, we used a correlation plot to further assess the strength and direction of this association.

Similar to the bar graph, the x-axis in the correlation plot represents the logged areas, while the y-axis represents the acres burned. Each point corresponds to a different logging area. Most data points fall between 0 and 500 logged acres, though some areas have up to approximately 2,500 logged acres. The plot suggests that smaller logged areas tend to have higher wildfire damage, whereas larger logged areas show less damage. Since the trend line has a negative slope, it indicates a potential inverse relationship, suggesting that increased logging activity may be associated with reduced wildfire damage.

In addition to the plot, correlation statistics provide insight into the strength of the relationship between the two variables. Figure 6 visually shows a slight downward slope, but the statistical data helps us interpret the correlation more accurately. This information is important in evaluating our hypothesis and understanding whether a meaningful relationship exists between logging activity and wildfire severity.

Our correlation coefficient was -0.13 (Table 2), indicating little to no relationship between the two variables. Additionally, the p-value was 0.5923, which is greater than 0.05. A higher p-value typically suggests a weak correlation that is not statistically significant, meaning the observed relationship could be due to random chance. Overall, more data is needed to determine whether there is a meaningful relationship .

**Conclusion**


While wildfires are natural events, their increasing severity and frequency pose significant concerns for California’s ecosystems and communities. Our report examines the relationship between logging and wildfire activity. Using various R coding techniques, data wrangling, and mapping methods, we found no strong correlation between these two variables. While our analysis provides insight into the distribution of both logging activity and wildfire incidents, additional data and research are needed to reach a definitive conclusion.

It is important to note that logging is not the only factor influencing fire frequency and severity. Other variables, such as vegetation changes and climate change, may have altered environmental conditions, making certain areas more fire-prone. For future research, we aim to incorporate more detailed logging statistics and investigate additional factors that contribute to wildfires, such as vegetation density, climate change, and wind speed.

