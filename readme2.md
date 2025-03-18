       Connection Between Wildfires and Logging Activity in California
Question: Does logging activity affect wildfire occurrences? 
Hypothesis: In areas with higher logging activity, the acres burned from wildfires will increase.

Description / Background:
   Wildfires are a natural and essential part of many Californian ecosystems, playing a key role in maintaining balance by supporting new growth and recycling nutrients. However, their continuous impact on human infrastructure, communities, and threats to the ecosystem can be devastating. For our project, we want to explore the connections between wildfires and logging within California’s national forests. 
Specifically, California has been known to have the most wildfires in terms of the number of wildfires and acres burned. As we are impacted directly, these fires are devastating for many residents living near wildfire events, and due to the recent events of the Los Angeles fires, researching the potential factors causing these events is essential for proposing solutions. By investigating the logging activities within the national forests, we should be able to see the relationship between the wildfires and the amount of trees in those areas. 
California wildfires are due to the significant climate change factors of warmer temperatures and reduced precipitation. With less precipitation, vegetation within the national forests will become dry, increasing the possibility of potential ignition. From 2020 to 2023, the annual average number of acres burned was about three times higher than in 2010 (OEHHA 2024). Although wildfires are a natural occurrence and will continue to be part of California’s landscape, logging practices have proceeded to remove most fire-resistant trees for timber, leaving young shrubs and saplings that are more susceptible to ignition, to fight for themselves (CALWILD 2025). Logging also increases fire risk by assembling treetops, needles, leaves, branches/limbs, and other parts of trees that are not collected, on the ground escalating the potential fuel available for the next wildfire (‘Salvage Logging - Sierra Forest Legacy’ 2025, para. 2). Observing and understanding how logging activities influence wildfire spread can help mitigate fire risks while balancing economic and environmental priorities.










Description of the dataset and exploratory data analysis (variable distribution, correlation, etc), (~1-3 plots OR data tables plus 3-4 paragraphs)

In this project, we used two datasets, one from the USDA Forest Service and the other from the California Wildfire Service. The USDA Forest Service’s dataset contains over 200 years worth of data on timber harvests within national forests in the United States.  The USDA Timber Harvest contained information about the logging activity within national forests such as the number of acres, logging technique, and land suitability. The California Wildfire Service dataset on the other hand, contains data and information from 2013 to 2020, including the location of wildfires, county names, latitude/longitude values, and the number of acres burned. As our research focuses on California’s National Forests, we must filter and refine the datasets to ensure that our analysis accurately reflects the environment of these forests.
The Timber Harvest dataset consists of 70 variables describing different factors of forest characteristics and the activities that take place within.  In order to explore and utilize the data effectively, research on the different variables was needed so that when they were used within the code, we would reference the correct column. After reviewing the dataset, we identified key variables that were relevant for our analysis. Specifically, we wanted to focus on the variables that were related to the location of the logging practices and the year in which the logging practice was completed. SImilarly, for the wildfire data set, we focused on the variables that were related to the number of acres burned, location of the fire within California's national forests, and the year in which it occurred. By aligning these variables, we aim to examine potential relationships between timber harvesting and wildfire susceptibility.
To understand the information provided by these datasets, we created two separate maps illustrating California containing logging activity (Figure 1) and wildfires (Figure 2). Figure 1 illustrates the spatial distribution of logging activity throughout California in 2019, where the blue polygons represent the areas where logging occurred. The distribution of logging activity significantly varies between forests as some have experienced minimal timber harvests while others faced substantial amounts of logging activities (Figure 3). The wildfire data contains information on the increasing number of acres burned over the course of about 200 years. Initial findings suggest that with the increased number of logging practices within the national forests, the more susceptible the land will be to wildfires.
    Figure 1 represents the logging practices within California National Forests. We can see that most of the logging activity takes place within Northern California. In comparing both Figure 1 and Figure 2, we can observe that the center of the state contains less logging activity, but also the majority of wildfire incidents. 







More detailed plots / statistical analysis (3-4 plots OR regressions OR tables + about 6-7 paragraphs of writing) 

Mapping logging regions and wildfire incidents provides a clear way to identify areas with activity, but our goal was to determine whether logging had any impact on wildfire occurrence. To analyze this, we used the merged dataset to examine spatial areas that had experienced both logging and wildfires during the same period. Figure 4 presents a visual representation of the overlap between logging/timber harvest activity and wildfire incidents. This overlap is particularly noticeable in Northern California, near the state lines bordering Northwestern Nevada and Southern Oregon. To highlight this, it is marked with a circled yellow dot.
The bar graph visually represents the extent of wildfire damage by showing the total number of acres burned in different logged areas. The data is arranged in descending order to make patterns easier to identify. The vertical (y-axis) represents the total acres burned, while the horizontal (x-axis) represents the logged areas within California’s national forests. From this, we can clearly see that some regions were affected more than others. Notably, Plumas National Forest experienced the highest number of acres burned in logged areas, whereas Modoc National Forest had the lowest. This plot highlights how wildfire severity varies across different regions, even among forests in close proximity within Northern California.


A correlation plot helps us determine whether two variables are related and visually represents the strength of their correlation. An upward trend typically indicates a positive relationship, while a downward trend suggests a negative correlation. If the data points form a straight line with no clear trend, it implies little to no relationship between the variables. While our initial findings suggested a connection between logging intensity and wildfire severity, we used a correlation plot to further assess the strength and direction of this association.
Similar to the bar graph, the x-axis in the correlation plot represents the logged areas, while the y-axis represents the acres burned. Each point corresponds to a different logging area. Most data points fall between 0 and 500 logged acres, though some areas have up to approximately 2,500 logged acres. The plot suggests that smaller logged areas tend to have higher wildfire damage, whereas larger logged areas show less damage. Since the trend line has a negative slope, it indicates a potential inverse relationship, suggesting that increased logging activity may be associated with reduced wildfire damage.
In addition to the plot, correlation statistics provide insight into the strength of the relationship between the two variables. Figure 6 visually shows a slight downward slope, but the statistical data helps us interpret the correlation more accurately. This information is important in evaluating our hypothesis and understanding whether a meaningful relationship exists between logging activity and wildfire severity.
Our correlation coefficient was -0.13 (Table 2), indicating little to no relationship between the two variables. Additionally, the p-value was 0.5923, which is greater than 0.05. A higher p-value typically suggests a weak correlation that is not statistically significant, meaning the observed relationship could be due to random chance. Overall, more data is needed to determine whether there is a meaningful relationship .

Correlation Plot and Result: 
```{r}
library(tidyr)
library(dplyr)
library(sf)
library(terra)
library(lubridate)
library(ggplot2)
library(stars)


# Replace with actual file path
logging_sf <- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/Actv_TimberHarvest/S_USA.Actv_TimberHarvest.shp")  
fire_crs <- read.csv("C:/Users/candl/OneDrive/Desktop/esp prject/California_Fire_Incidents.csv")
ca <- st_read("C:/Users/candl/OneDrive/Desktop/esp prject/ca_state/CA_State.shp")


fire_sf <- st_as_sf(fire_crs, coords = c("Longitude", "Latitude"), crs = 4326)

logging_sf <- st_transform(logging_sf, st_crs(ca))
fire_sf <- st_transform(fire_crs, st_crs(ca))

fire_sf <- st_transform(fire_sf, st_crs(logging_sf))

st_crs(fire_sf) <- 4326
fire_sf <- st_transform(fire_sf, st_crs(ca))

fire_logging_overlap <- st_join(fire_sf, logging_sf, left = FALSE, join = st_intersects)

--------- 
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





Conclusions

While wildfires are natural events, their increasing severity and frequency pose significant concerns for California’s ecosystems and communities. Our report examines the relationship between logging and wildfire activity. Using various R coding techniques, data wrangling, and mapping methods, we found no strong correlation between these two variables. While our analysis provides insight into the distribution of both logging activity and wildfire incidents, additional data and research are needed to reach a definitive conclusion.
It is important to note that logging is not the only factor influencing fire frequency and severity. Other variables, such as vegetation changes and climate change, may have altered environmental conditions, making certain areas more fire-prone. For future research, we aim to incorporate more detailed logging statistics and investigate additional factors that contribute to wildfires, such as vegetation density, climate change, and wind speed.


