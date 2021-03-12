# Dellena Bloom
# March 12th, 2021
# Mapping

# load packages
library(tidyverse)
library(ggmap)
library(maps)
library(sf)
library(mapview)
library(rgdal)
library(sp)
# Dellena file path
data_directory <- '~/Documents/Kueppers lab'

# load data
NDVI <- read.csv(file.path(data_directory, "new_Disturbance_details_NDVI.csv"), stringsAsFactors = FALSE)
register_google(key = "[AIzaSyC1GhuoSLcuRfBabIwHYWVrVb3DtU-LcP0]")


# standardize change in litterfall
#NDVI$TL_change_1 <- scale(NDVI$TL_change_1)
#NDVI$LL_change_1 <- scale(NDVI$LL_change_1)
#NDVI$TL_change_2 <- scale(NDVI$TL_change_2)
#NDVI$LL_change_2 <- scale(NDVI$LL_change_2)

# Data manipulation
#CL <- coordinates(NDVI)<-~Longitude+Latitude # whatever the equivalent is in your 
qmplot(NDVI$Longitude, NDVI$Latitude)

# Map NDVI
qmplot(Longitude, Latitude, data = NDVI$NDVI1, colour = I('red'), size = I(3), darken = .3)

map <- get_map(location = 'Europe', zoom = 4)
ggmap(map) + 
  geom_point(data = NDVI, aes(x = Longitude, y = Latitude))

