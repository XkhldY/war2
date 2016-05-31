library(leaflet)


directory="D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data"
setwd("D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data")
dirs = list.files(directory)
dir = paste(dirs[4], "Trajectory", sep = "/")

z= NULL
i=0
f = list.files(dir, pattern = "*.plt",full.names = TRUE)[5]
tab=data.frame(read.table(sep = ",",f,header = FALSE,,skip = 6,stringsAsFactors=FALSE))

names(tab$V1)<-"lat"
names(tab$V2)<-"long"
colnames(tab)[1]<-"lat"
colnames(tab)[2]<-"long"
# tab$date = as.POSIXct(paste(tab[,8],tab[,9]),tz = "GMT") 
# attributes(tab$date)$tzone <- "Asia/Shanghai"
# tab$hour=hour(tab$date)
# 
# ll=cbind(tab$Long,tab$Lat)
# xy = mercator(ll)#/10000000


m <- leaflet()
m <- addTiles(m)
# m <- addMarkers(m, lng=tab[tab$UserID==2,"Long"], lat=tab[tab$UserID==2,"Lat"])
m <- addCircleMarkers(m, lng=tab$long, lat=tab$lat,radius = 1)

m
#  (radius = ~ifelse(type == "ship", 6, 10),color = ~pal(type),stroke = FALSE, fillOpacity = 0.5)
# m <- addMarkers(m, qq2[,1],qq2[,2],popup = "stay")
# m <- addCircles(qq2[,1], qq2[,2], radius = runif(50, 50, 150))

# 
# leaflet(tab[,c("lat","long")]) %>% addTiles() %>% addMarkers(
#   clusterOptions = markerClusterOptions()
# )