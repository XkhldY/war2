library("flexclust")
library("geosphere")
library("ggmap")
library("ggplot2")
library("fossil")
library(LPCM)
library("lubridate")
directory="D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data"
setwd("D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data")
dirs = list.files(directory)
dir = paste(dirs, "Trajectory", sep = "/")

StayPoints_temp=NULL
k=4
for(k in 1:10)
{
  StayPoints=NULL
  id=k
    for (f in list.files(dir[k], pattern = "*plt", full.names = TRUE))
    {
     
      
#       sub=as.numeric(substr(f,start =1 ,stop = 3))
      
      print(f)
    
      tab=data.frame(read.table(sep = ",",f,skip=6,stringsAsFactors=FALSE))
    
      tab$date=paste(tab[,6],tab[,7])
      tab = tab[,-c(3,4,6,7)]
       names(tab) = c("lat", "long","date1","date2")

    
    tab1=data.frame(tab$long,tab$lat)
    i=1
 
    
    while (i <nrow(tab1))
    {
      j=i+1
      Stay_temp=NA
      Stay_temp=as.data.frame(rbind(Stay_temp,tab[i,]))
      while(j<nrow(tab1))
      {
        dist_diff=distm(tab1[j,],tab1[i,])
        if(dist_diff<=200)
        {
          Stay_temp=as.data.frame(rbind(Stay_temp,tab[j,]))
        }
        else
        {
          Time_Diff=tab[j,]$date1-tab[i,]$date1
          if(Time_Diff>=0.013888889)
          {
            # qq=aggregate(mercator(Stay_temp[, 2:1]), list(Stay_temp$day), median)
            Stay_temp$ID=id
            Stay_temp$duration=(tab[j,]$date1-tab[i,]$date1)*24*60
            
            if(!is.na(Stay_temp$lat[1]))
            {
              Stay_temp$previous=StayPoints[nrow(StayPoints),]$ID
              Stay_temp$Dist_diff=distm(Stay_temp[nrow(Stay_temp),c("long","lat")],StayPoints[2,c("long","lat")])[1]
            }
            else
             {
               Stay_temp$Dist_diff=-1
               Stay_temp$previous=-1
             }
            
            midpoint=midPoint(Stay_temp[nrow(Stay_temp),c("long","lat")], Stay_temp[nrow(Stay_temp),c("long","lat")])
            Stay_temp[1,]$long=midpoint[1]
            Stay_temp[1,]$lat=midpoint[2]
            Stay_temp$file=f
            
              

            StayPoints=as.data.frame(rbind(StayPoints,Stay_temp[1,]))
            # StayPoints_temp=Stay_temp
            id=id+1
          }
          i=j
          break
        }
        
        j=j+1
      }
      
      i=j+1
    }
    

      x=paste("D:/work/Research/Rstaypoints/",k,".txt")
      Sfile=gsub(" ","",x)
      print("number of staypoints in user: ",k)
      
      print(nrow(StayPoints))
      write.table(x = StayPoints,file = Sfile,row.names = FALSE,sep = ",")



    }
  
  if(!is.null(StayPoints))
  {
  StayPoints_temp=as.data.frame(rbind(StayPoints_temp,StayPoints))
  
  write.table(x = StayPoints_temp,file = "D:/work/Research/Rstaypoints/all.txt",row.names = FALSE,sep = ",")
  }
#   k=k+122


}

m <- leaflet()
m <- addTiles(m)
# m <- addMarkers(m, lng=tab[tab$UserID==2,"Long"], lat=tab[tab$UserID==2,"Lat"])
m <- addCircleMarkers(m, lng=StayPoints$long, lat=StayPoints$lat,radius = 10,fillColor = "red")

m

     # plot(xy,sub=f)
     # points(xy[1:700,], type = "p",col = "blue", cex = 1,pch = 19)
#     

