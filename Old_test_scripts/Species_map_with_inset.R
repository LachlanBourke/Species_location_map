#Species location map
#Author: Lachlan Bourke
#2022

#Packages
library(tmap) #tmap book https://r-tmap.github.io/tmap-book/layout.html
library(sf)
library(rnaturalearth)
library(rnaturalearthhires) # devtools::install_github("ropensci/rnaturalearthhires")

#venom sample location data frame
dat <- data.frame(Locality=c("A", "B", "C", "D", "E"), 
                  Latitude=c(-4.12537, -3.33078, -3.90854, -1.37069, -0.0454), 
                  Longitude=c(-79.19761, -79.10053, -78.48923, -78.0481, -78.76075), 
                  Type=c("B. lojanus (1)", "B. lojanus (2)","B. microphthalmus (1)","B. microphthalmus (2)", "B. campbelli" ))

#Convert data frame to simple features (sf). See https://www.r-bloggers.com/2019/10/make-a-map-of-your-study-site-with-tmap/#google_vignette
#uses library sf
sites <- st_as_sf(dat, coords = c("Longitude", "Latitude"))

####Using natural earth package to get Ecuador map data###########
ECU<-ne_states(country = "ecuador",returnclass = "sf")

#View names
ECU$name

#change state names to abbreviations (GOOGLE ECUADOR PROVINCES ABBREVIATIONS)
ECU$name[ECU$name=="Esmeraldas"] <- "E"
ECU$name[ECU$name=="Sucumbios"] <- "U"
ECU$name[ECU$name=="Pastaza"] <- "Y"
ECU$name[ECU$name=="Zamora Chinchipe"] <- "Z"
ECU$name[ECU$name=="El Oro"] <- "O"
ECU$name[ECU$name=="Manabi"] <- "M"
ECU$name[ECU$name=="Cañar"] <- "F"
ECU$name[ECU$name=="Tungurahua"] <- "T"
ECU$name[ECU$name=="Bolivar"] <- "B"
ECU$name[ECU$name=="Cotopaxi"] <- "X"
ECU$name[ECU$name=="Pichincha"] <- "P"
ECU$name[ECU$name=="Carchi"] <- "C"
ECU$name[ECU$name=="Orellana"] <- "D"
ECU$name[ECU$name=="Morona Santiago"] <- "S"
ECU$name[ECU$name=="Loja"] <- "L"
ECU$name[ECU$name=="Guayas"] <- "G"
ECU$name[ECU$name=="Santa Elena"] <- "SE"
ECU$name[ECU$name=="Azuay"] <- "A"
ECU$name[ECU$name=="Napo"] <- "N"
ECU$name[ECU$name=="Chimborazo"] <- "H"
ECU$name[ECU$name=="Imbabura"] <- "I"
ECU$name[ECU$name=="Los Rios"] <- "R"
ECU$name[ECU$name=="Santo Domingo de los Tsáchilas"] <- "SD"


####Alternative way to get map data - use geodata package to get GADM data)####
#Library raster used to get shapefile data off of GADM
#tmap used to plot
#ECU<- geodata::gadm(country = "ECU", level = 1, path = "GADM_data", version = "latest", resolution = 1, type = "sf")
#ECU
#Convert spatvector to shapefile so I can plot it with tmap
#ECU<-st_as_sf(ECU)
#View names 
#ECU@data[["NAME_1"]]
#change state names to abbreviations (GOOGLE ECUADOR PROVINCES ABBREVIATIONS)
#ECU$NAME_1[ECU$NAME_1=="Azuay"] <- "A"
#ECU$NAME_1[ECU$NAME_1=="Bolivar"] <- "B"
#................etc.

####Making a detailed map of Ecuador (with sites) from the data####

#Area to map (excludes galapagos)
bbox_ec <- st_bbox(c(xmin = -82, xmax = -70, ymax = 2, ymin = -6.5), crs = st_crs(4326))

#code below modified from https://www.r-bloggers.com/2019/10/make-a-map-of-your-study-site-with-tmap/#google_vignette
ECU_map <- tm_shape(ECU, bbox=bbox_ec) + tm_polygons(col="green4", alpha=0.5,legend.show = F)+
  tm_text("name", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.1,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=1, col = "Type",
             title.col = "Venom",
             palette = "Reds")+
  tm_layout(legend.position = c(0.6,0.65),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 0.84, legend.text.fontfamily = "Times", legend.text.fontface = "italic",
            frame = T)
ECU_map


####Making a continent map with Ecuador coloured in####

SA_naturalearth <- ne_countries(continent = "South America", returnclass = "sf")
ECU_naturalearth <-ne_states(country = "ecuador",returnclass = "sf") #Another Ecuador map

SA_map <-
  tm_shape(SA_naturalearth)+
  tm_polygons(col = "grey",alpha=0.4, border.col = "black")+
  tm_shape(ECU_naturalearth)+tm_fill(col = "green4")+tm_layout(frame = T)

SA_map


####Inset map####
#Using code from https://geocompr.robinlovelace.net/adv-map.html 

library(grid)
ECU_map
print(SA_map, vp = viewport(0.7, 0.3, width = 0.5, height = 0.5))

tmap_save(ECU_map,insets_tm = SA_map, insets_vp = viewport(0.65, 0.26, width = 0.5, height = 0.5))

