#https://www.albinfontaine.com/post/making-maps-in-r/
#http://bboxfinder.com/#0.000000,0.000000,0.000000,0.000000

library(leaflet)
library(ggmap)
library(ggrepel)
library(ggsn)
library(rgdal)

# Define bounding box with longitude/latitude coordinates
#Use this code to choose you long and lat values. It is interactive

bbox <- list(
  p1 = list(long = -80.395340, lat = -4.975011),
  p2 = list(long = -75.428793, lat = 1.804361)
)

leaflet() %>%
  addTiles() %>% 
  addRectangles(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
    fillColor = "transparent" ) %>%
  fitBounds(
    lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
    lng2 = bbox$p2$long, lat2 = bbox$p2$lat  )

#create data frame
dat <- data.frame(Locality=c("A", "B"), 
                  Latitude=c(-4.12537, -3.33078), 
                  Longitude=c(-79.19761, -79.10053), 
                  Type=c("B. lojanus", "B. lojanus1"))
dat


###############

#Get satelite map
#change "source = "to change map type. https://colauttilab.github.io/EcologyTutorials/ggmapCheatsheet.pdf
Map <- get_map(location=c(left = bbox$p1$long, 
                          bottom = bbox$p1$lat,
                          right = bbox$p2$long, 
                          top = bbox$p2$lat), 
               source="osm", color = "bw")
ggmap(Map)

myColors <- c("black","darkorange")
names(myColors) <- levels(factor(dat$Type, levels=c("B. lojanus", "B. lojanus1")))
colScale <- scale_color_manual(name = "Type",values = myColors)
map <- ggmap(Map)
map <- map + coord_fixed() + theme_minimal(base_size = 20)

map <- map + ggtitle("Study area") + colScale
map <- map + geom_point(data=dat, aes(x=Longitude, y=Latitude, color=Type),size=5, alpha=0.9)
map <- map + xlab("Longitude")+ ylab("Latitude")
map <- map + north(location = "topleft", x.min = bbox$p1$long, x.max = bbox$p2$long, y.min = bbox$p1$lat, y.max = (bbox$p2$lat-0.01))
map <- map + ggsn::scalebar(x.min = bbox$p1$long, x.max = bbox$p2$long-0.02, y.min = (bbox$p1$lat+0.01), y.max = bbox$p2$lat, location = "bottomright",transform = TRUE, dist = 2,dist_unit="km", model = "WGS84")
map

require(svglite)
svglite("Map.svg", width = 10,height = 10)
print(map)
dev.off()





################################
#Following cheat sheet
myLocation <- "University of Washington"

myLocation <- c(lon = -95.3632715, lat = 29.7632836)

myLocation <- c(-130, 30, -105, 50)

myMap <- get_map(location=myLocation, source="stamen", maptype="toner",crop=FALSE)
ggmap(myMap)

myMap <- get_stamenmap(location=myLocation, maptype="terrain",crop=FALSE)
ggmap(myMap)



