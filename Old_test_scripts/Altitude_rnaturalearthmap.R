#Altitude map
#Author: Lachlan Bourke
#2024

#### geodata ####
#https://cran.r-project.org/web/packages/geodata/geodata.pdf
#See if can get other data with geodata. soil, veg????

library(geodata)
#Country with elevation data
ECU.elevation<-elevation_30s("ECU",path = "Altitude_map_data", mask = T)
plot(ECU.elevation)

#### Make plot ####
library(tmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires) # devtools::install_github("ropensci/rnaturalearthhires")

# sites
#venom sample location data frame
dat <- data.frame(Locality=c("A", "B", "C", "D", "E"), 
                  Latitude=c(-4.12537, -3.33078, -3.90854, -1.37069, -0.0454), 
                  Longitude=c(-79.19761, -79.10053, -78.48923, -78.0481, -78.76075), 
                  Type=c("B. lojanus (Loja)", "B. lojanus (Azuay)","B. microphthalmus (Zamora Chinchipe)","B. microphthalmus (Pastaza)", "B. campbelli" ))
                    #update lat long (more decimals in excel)

#Convert data frame to simple features (sf).
sites <- sf::st_as_sf(dat, coords = c("Longitude", "Latitude"))

####Using natural earth package to get Ecuador map data###########
ECU<-ne_states(country = "ecuador",returnclass = "sf")

#View names
ECU$name

##NOTE NAPO AND TUNGURAHUA MISTAKE IN DATA TABLE. 
#N is where T should be and vice versa.FIXED when changed names below. 

#change state names to abbreviations (GOOGLE ECUADOR PROVINCES ABBREVIATIONS)
ECU$name[ECU$name=="Esmeraldas"] <- "E"
ECU$name[ECU$name=="Sucumbios"] <- "U"
ECU$name[ECU$name=="Pastaza"] <- "Y"
ECU$name[ECU$name=="Zamora Chinchipe"] <- "Z"
ECU$name[ECU$name=="El Oro"] <- "O"
ECU$name[ECU$name=="Manabi"] <- "M"
ECU$name[ECU$name=="Cañar"] <- "F"
ECU$name[ECU$name=="Tungurahua"] <- "N"
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
ECU$name[ECU$name=="Napo"] <- "T"
ECU$name[ECU$name=="Chimborazo"] <- "H"
ECU$name[ECU$name=="Imbabura"] <- "I"
ECU$name[ECU$name=="Los Rios"] <- "R"
ECU$name[ECU$name=="Santo Domingo de los Tsáchilas"] <- "SD"

#Area to map (excludes galapagos) use http://bboxfinder.com
bbox_ec <- st_bbox(c(xmin = -81.5, xmax = -70, ymax = 2, ymin = -6.5), crs = st_crs(4326))

#code below modified from https://www.r-bloggers.com/2019/10/make-a-map-of-your-study-site-with-tmap/#google_vignette
####Map1####
ECU_map <- tm_shape(ECU.elevation, bbox=bbox_ec) + 
  tm_raster(title = "Elevation") + #ECU.elevation is raster
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("name", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.5, col = "Type", #?pch to see symbols and corresponding numbers
             title.col = "Venom",
             palette = "Reds",
             shapes = "Type") +
  tm_layout(legend.position = c(0.55,0.05),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 0.84, legend.text.fontfamily = "Times", legend.text.fontface = "italic",
            frame = T) #Frame T is useful to orientate things

ECU_map

####Map2####
#Want to change symbol shape, but then makes two legends so add manual legend
#https://stackoverflow.com/questions/46702218/how-can-i-make-a-legend-show-size-and-color-for-tm-bubbles-in-r

ECU_map <- tm_shape(ECU.elevation, bbox=bbox_ec) + 
  tm_raster(title = "Elevation") + #ECU.elevation is raster.
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("name", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.3, shape = "Type",
             title.shape = "Venom",
             shapes = c(21,22,23,24,25),
             col = "Type", palette = "OrRd",
             legend.shape.show =F, legend.col.show = F) +
  tm_add_legend("symbol", col="green", #edit col to match symbols
                shape=c(21,22,23,24,25), 
                labels=c("B. campbelli","B. lojanus (Azuay)",
                         "B. lojanus (Loja)", "B. microphthalmus (Pastaza)",
                         "B. microphthalmus (Zamora Chinchipe)"), 
                title = "Venom")+
  tm_layout(legend.position = c(0.6,0.05),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 0.84, legend.text.fontfamily = "Times", legend.text.fontface = "italic",
            frame = F) #Frame T is useful to orientate things

ECU_map

####Map3####
#Use terrain 2 colour palette from colorspace
#https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html
library(colorspace) #get terrain 2 color palette
hcl_palettes()
palette <- sequential_hcl(8, palette="Terrain 2")
print(palette) #get palette colours, manually add in so can change first colour

ECU_map <- tm_shape(ECU.elevation, bbox=bbox_ec) + 
  tm_raster(title = "Elevation", 
            palette = c("#A7DFD2","#75AE18","#A6B40C","#CFB938","#F3BE65",
                        "#FFC393","#FFCBC0","#F1F1F1")) + #ECU.elevation is raster.
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("name", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.5, shape = "Type",
             title.shape = "Venom",
             shapes = c(21,22,23,24,25),
             col = "red") +
  tm_layout(legend.position = c(0.6,0.05),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 0.84, legend.text.fontfamily = "Times", legend.text.fontface = "italic",
            frame = F) #Frame T is useful to orientate things

ECU_map
tmap_save(ECU_map)

####Map4####
#Make my own palette
library(grDevices)
pal <- colorRampPalette(c("#35d448", "brown"))
pal(8)

ECU_map <- tm_shape(ECU.elevation, bbox=bbox_ec) + 
  tm_raster(title = "Elevation", 
            palette = c("#A7DFD2","#35D448","#45BB43","#658B3B","#757236",
                        "#845A32", "#95422E", "#A52A2A")) + #ECU.elevation is raster. Can change palette here
  tm_shape(ECU)+
  tm_polygons(alpha=0)+ #what ECU consists of 
  tm_text("name", size = 1, alpha = 0.5, fontfamily = "Times")+
  tm_scale_bar(position = c(0.08,0), text.size = 0.9)+
  tm_shape(sites)+
  tm_symbols(size=0.5, shape = "Type",
             title.shape = "Venom",
             shapes = c(21,22,23,24,25),
             col = "grey") +
  tm_layout(legend.position = c(0.6,0.05),
            legend.title.size = 1.5, legend.title.fontfamily = "Times",
            legend.text.size = 0.84, legend.text.fontfamily = "Times", legend.text.fontface = "italic",
            frame = F) #Frame T is useful to orientate things

ECU_map




