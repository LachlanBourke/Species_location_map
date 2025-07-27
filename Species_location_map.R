#Altitude map
#Author: Lachlan A. Bourke
#2025

#### geodata ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf

library(geodata)

# Download map data and plot other data over the map
#Use geodata package. 

#Country with province borders
ECU<- geodata::gadm(country = "ECU", level = 1, path = "Country_data", version = "latest", resolution = 1, type = "sf")

#Country with elevation data
ECU.elevation<-elevation_30s("ECU",path = "Altitude_map_data", mask = T)
plot(ECU.elevation)

#### Make plot
library(tmap)
library(sf)
#use below code to convert spatvector to simple features (sf) (can then map in tmap)
#Could just use rnaturalearth to download ECU map as it will be sf already. But since use geodata
#the map downloaded as a spatvector which tmap can't plot. 
ECU <- sf::st_as_sf(ECU)

#View names
ECU$NAME_1
#change state names to abbreviations 
#https://www.iso.org/obp/ui/#iso:code:3166:EC ISO 3166 code Abbreviations.
ECU$NAME_1[ECU$NAME_1=="Esmeraldas"] <- "E"
ECU$NAME_1[ECU$NAME_1=="Sucumbios"] <- "U"
ECU$NAME_1[ECU$NAME_1=="Pastaza"] <- "Y"
ECU$NAME_1[ECU$NAME_1=="Zamora Chinchipe"] <- "Z"
ECU$NAME_1[ECU$NAME_1=="El Oro"] <- "O"
ECU$NAME_1[ECU$NAME_1=="Manabi"] <- "M"
ECU$NAME_1[ECU$NAME_1=="Cañar"] <- "F"
ECU$NAME_1[ECU$NAME_1=="Tungurahua"] <- "T"
ECU$NAME_1[ECU$NAME_1=="Bolivar"] <- "B"
ECU$NAME_1[ECU$NAME_1=="Cotopaxi"] <- "X"
ECU$NAME_1[ECU$NAME_1=="Pichincha"] <- "P"
ECU$NAME_1[ECU$NAME_1=="Carchi"] <- "C"
ECU$NAME_1[ECU$NAME_1=="Orellana"] <- "D"
ECU$NAME_1[ECU$NAME_1=="Morona Santiago"] <- "S"
ECU$NAME_1[ECU$NAME_1=="Loja"] <- "L"
ECU$NAME_1[ECU$NAME_1=="Guayas"] <- "G"
ECU$NAME_1[ECU$NAME_1=="Santa Elena"] <- "SE"
ECU$NAME_1[ECU$NAME_1=="Azuay"] <- "A"
ECU$NAME_1[ECU$NAME_1=="Napo"] <- "N"
ECU$NAME_1[ECU$NAME_1=="Chimborazo"] <- "H"
ECU$NAME_1[ECU$NAME_1=="Imbabura"] <- "I"
ECU$NAME_1[ECU$NAME_1=="Los Rios"] <- "R"
ECU$NAME_1[ECU$NAME_1=="Santo Domingo de los Tsachilas"] <- "SD"

#Need to simplify as has polygons and multipolygons. If use above will get
#duplicate text values for the provinces.
#https://stackoverflow.com/questions/69947457/tm-text-produces-duplicate-text-in-tmap
library(dplyr)
ECU <- ECU %>%  
  st_cast()

# sites
#venom sample location data frame
#NOTE COORDINATES ARE RANDOM - THEY ARE NOT RELATED TO EACH LOCALITY.
#THIS IS TO KEEP THE EXACT LOCATIONS PRIVATE.
dat <- data.frame(Locality=c("A", "B", "C", "D", "E"), 
                  Latitude=c(-0.5983, -1.2456, 0.3211, -3.6543, -1.0123), 
                  Longitude=c(-78.5123, -79.8234, -77.345, -79.1234, -78.6543), 
                  Type=c("B. lojanus (Loja)", "B. lojanus (Azuay)","B. microphthalmus (Zamora Chinchipe)","B. microphthalmus (Pastaza)", "B. campbelli" ))

#Convert data frame to simple features (sf).
sites <- sf::st_as_sf(dat, coords = c("Longitude", "Latitude"))

#Area to map (excludes galapagos) use http://bboxfinder.com
bbox_ec <- st_bbox(c(xmin = -81.5, xmax = -70, ymax = 2, ymin = -6.5), crs = st_crs(4326))


#Fonts
#https://cran.r-project.org/web/packages/extrafont/readme/README.html
library(extrafont)
font_import() #import fonts isntalled on system
fonts() #vector of font names
loadfonts() #Register fonts

#Use terrain 2 colour palette from colorspace
#https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html
library(colorspace) #get terrain 2 color palette
hcl_palettes()
palette <- sequential_hcl(8, palette="Terrain")
print(palette) #get palette colours.
#manually add colours into code below so can change first colour.

#play around to get perfect bbox
bbox_ec <- st_bbox(c(xmin = -81.5, xmax = -71, ymax = 2, ymin = -6.5), crs = st_crs(4326))

#code below modified from https://www.r-bloggers.com/2019/10/make-a-map-of-your-study-site-with-tmap/#google_vignette
ECU_map <- tm_shape(ECU.elevation, bbox=bbox_ec) + 
  tm_raster(title = "Elevation (m)", 
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1")) + #ECU.elevation is raster.
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("NAME_1", size = 1, alpha = 0.5)+
  tm_scalebar(position = c(0.08,0.1), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.5, shape = "Type",
             title.shape = "Venom",
             shapes = c(21,22,23,24,25),
             col = "red") +
  tm_layout(text.fontfamily = "Times New Roman",legend.position = c(0.56,0.7),
            legend.title.size = 1.5, legend.text.size = 0.8, 
            legend.text.fontface = "italic",legend.frame = F,
            frame = F) #Frame T is useful to orientate things

ECU_map

#save
#https://r-tmap.github.io/tmap-book/save.html
tmap_save(ECU_map)#png
tmap_save(ECU_map,filename = "tmap.tiff", dpi = 600)


#Other ways to save

#Vector map
#tmap_save(ECU_map,"tmap.svg")

#Interactive map
#tmap_save(ECU_map,"interactive_tmap.html")

#require(svglite)
#svglite("ECU_map.svg", width = 10,height = 10)
#print(ECU_map)
#dev.off()
