#Altitude map
#Author: Lachlan Bourke
#2022

#### geodata ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf
#See if can get other data with geodata. soil, veg????

library(geodata)

# Download map data and plot other data over the map
#Use geodata package. 

#Country with province borders
ECU<- geodata::gadm(country = "ECU", level = 1, path = tempdir(), version = "latest", resolution = 1, type = "sf")

#Country with elevation data
ECU.elevation<-elevation_30s("ECU",path = ,tempdir(), mask = T)
plot(ECU.elevation)

#### TEST TERRA PACKAGE ####
#https://dominicroye.github.io/en/2022/hillshade-effects/
#https://www.r-bloggers.com/2022/10/hillshade-colors-and-marginal-plots-with-tidyterra-i/
library(raster)
library(terra)
slope = terra::terrain(ECU.elevation,v="slope",unit = "degrees")
plot(slope)
aspect = terra::terrain(ECU.elevation,v="aspect", unit = "radians")
plot(aspect)
#angle - the elevation angle of the light source (sun) in degrees
#direction - direction (azimuth) angle of the light source (sun) in degrees
#normalize Logical. If TRUE, values below zero are set to zero and the results are multiplied with 255
hill = terra::shade(slope, aspect,angle = 45, direction =300, normalize = TRUE)
plot(hill)

#Smaller area of Ecuador

#crop (give extent to crop)
ext1<-crop(ECU.elevation,ext(-80,-76,-2,0))
plot(ext1)

slope_ext1 = terra::terrain(ext1,v="slope",unit = "degrees")
plot(slope_ext1)
aspect_ext1 = terra::terrain(ext1,v="aspect", unit = "radians")
plot(aspect_ext1)
hill_ext1 = terra::shade(slope_ext1, aspect_ext1,angle = 15, direction =1000, normalize = TRUE)
plot(hill_ext1)

#### elavtr package ####
library(sf)
ECU <- sf::st_as_sf(ECU)
library(dplyr)
ECU <- ECU %>%  
  st_cast()

library(elevatr)
test1<-get_elev_raster(ECU, z=5)

plot(test)

library(tmap)
bbox_ec <- st_bbox(c(xmin = -81.5, xmax = -71, ymax = 2, ymin = -6.5), crs = st_crs(4326))

#code below modified from https://www.r-bloggers.com/2019/10/make-a-map-of-your-study-site-with-tmap/#google_vignette
ECU_map <- tm_shape(test1, bbox=bbox_ec) + 
  tm_raster(title = "Elevation (m)", 
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1")) + #ECU.elevation is raster.
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("NAME_1", size = 1, alpha = 0.5)+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_layout(fontfamily = "Times New Roman",legend.position = c(0.6,0.05),
            legend.title.size = 1.5, legend.text.size = 0.84, 
            legend.text.fontface = "italic",
            frame = F) #Frame T is useful to orientate things

ECU_map
