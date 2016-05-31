library("ggmap")
library("ggplot2")
library("flexclust")
library("geosphere")

directory="D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data"
setwd("D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data")

dirs = list.files(directory)

dir = paste(dirs[1], "Trajectory", sep = "/")
z= NULL
timeMean=NULL
#   z=as.data.frame(rbind(z,read.table(sep = ",",f,skip=6,stringsAsFactors=FALSE)))
for (f in list.files(dir, pattern = "*plt", full.names = TRUE))
{


    z=as.data.frame(read.table(sep = ",",f,skip=6,stringsAsFactors=FALSE))
    
    
    z$date = as.POSIXct(paste(z[,6],z[,7]),tz = "GMT") 
    rob
    
    z = z[,-c(3,4,5,6)]
    names(z) = c("lat", "long","time", "date")
    
    tdiff=sapply(2:nrow(z),function(i){difftime(z[i,]$date,z[i-1,]$date)})
    timediff=as.data.frame(rbind(timediff,tdiff))
    
    timeMean=as.data.frame(rbind(timeMean,mean(tdiff)))
    
}
# 
# attributes(z$date)$tzone <- "Asia/Shanghai"
# 
# day=as.Date(z$date)
# z$day=weekdays(day)
# write.table(z, file = "D:/work/Research/location research/trajectory1.csv",row.names=FALSE, sep=",")
