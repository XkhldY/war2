library(RPostgreSQL)
library("LPCM")
library("flexclust")
library("geosphere")
library("ggmap")
library("ggplot2")
library("fossil")
library(shapefiles)



f="D:/StayPoints.txt"
tab=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))


dd <- data.frame(Id=seq(1,length(tab$Long),by=1),X=tab$Long,Y=tab$Lat)
ddTable <- data.frame(Id=seq(1,length(tab$Long),by=1),tab$time)
ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
write.shapefile(ddShapefile, "D:/test1", arcgis=T)
