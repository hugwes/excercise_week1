
### EXCERCISE 1 ###

##########################################################
# Preparation
R.version.string # check the R version
library(tidyverse) 

##########################################################
# Task 1: Import Data
wildschwein <- read_csv("~/Desktop/ZHAW/Patterns & Trends (3kp)/Excercises/excercise_week1/wildschwein_BE.csv")
attr(wildschwein$DatetimeUTC,"tzone") # check timezone
head(wildschwein)
str(wildschwein)
summary(wildschwein)

##########################################################
# Task 2: Explore Data
ggplot(wildschwein, aes(Long, Lat, colour = TierID)) +
  geom_point(size=0.7) +
  theme(legend.position = "none")

# Input: Handling spatial Data
library(sf)
wildschwein <- st_as_sf(wildschwein, coords=c("Long", "Lat"), crs=4326) # convert into sf (spatial object, a type of dataframe)   
wildschwein

##########################################################
# Task 3: Project data from WGS84
wildschwein <- st_transform(wildschwein, 2056) # 2056 -> CH1903+ LV95
wildschwein

# Input: Calculate Convex Hull
wildschwein_grouped <- group_by(wildschwein,TierID) # add a grouping variable to the sf object 
wildschwein_grouped

wildschwein_smry <- summarise(wildschwein_grouped) # dissolve all points into a multipoint object
wildschwein_smry

mcp <- st_convex_hull(wildschwein_smry) # run the Convex Hull on the sf object (mcp = minimal convex polygone: a simple home range)

##########################################################
# Task 4: Ploting spatial objects
plot(mcp) # Plot with base R 

ggplot(data=mcp) + # Plot mit ggplot
  geom_sf(aes(fill = TierID), alpha = 0.4) + # fill --> Transparenz
  coord_sf(datum = 2056) # datum --> Koordinatenachsen anpassen

ggplot(mcp, aes(fill=TierID)) + # Plot mit ggplot
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056)

# Input: Importing Raster Data
library(terra)
pk100_BE <- terra::rast("pk100_BE_2056.tiff")
pk100_BE # this contains 3 layers
plot(pk100_BE) # each layer is displayed individually
plotRGB(pk100_BE) # all 3 layers are combined into a single image

##########################################################
# Task 5: Adding a Background Map
library(tmap) # thematic map (syntax is very similar to ggplot2)

tm_shape(pk100_BE) + 
  tm_rgb()

tm_shape(pk100_BE) + 
  tm_rgb() +
  tm_shape(mcp) +
  tm_polygons(col="TierID", alpha=0.4, border.col="red") +
  tm_legend(bg.color="white")

##########################################################
# Task 6: Create an interactive map
tmap_mode("view")

tm_shape(mcp) +
  tm_polygons(col = "TierID",alpha = 0.4,border.col = "red") +
  tm_legend(bg.color = "white")

##########################################################