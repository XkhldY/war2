library("flexclust")
library("geosphere")
library("ggmap")
library("ggplot2")
library("fossil")
library(LPCM)
library(dbscan)
library("lubridate")
directory="D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data"
setwd("D:/work/Research/location research/datasets/Geolife/Geolife Trajectories 1.3/Data")
dirs = list.files(directory)
dir = paste(dirs[61], "Trajectory", sep = "/")

z= 0
StayPoints2=NULL
StayPoints=NULL

for (f in list.files(dir, pattern = "*plt", full.names = TRUE))
{
  
  
  print(f)
  
  tab=data.frame(read.table(sep = ",",f,skip=6,stringsAsFactors=FALSE))
  #     if(nrow(tab)<240)
  #       next;
  #     tab = tab[,-c(3,4,5)]
  #     
  #     names(tab) = c("lat", "long","date","time")
  #     
  tab$date = as.POSIXct(paste(tab[,6],tab[,7]),tz = "GMT") 
  tab = tab[,-c(3,4,5,6)]
  names(tab) = c("lat", "long","time", "date")
  
  
  # attributes(tab$date)$tzone <- "Asia/Shanghai"
  tab$date=force_tz(tab$date, tzone = "Asia/Shanghai")
  #     day=as.Date(tab$date)
  tab$day=weekdays.POSIXt(tab$date,abbreviate = FALSE)
  
  #trajectories from Beijing
#   if(tab$lat<39.45 ||tab$lat>41 || tab$long<115.43 ||tab$long>117.40)
#     next;
  
  #removing transportaion mode    
  # start.time <- Sys.time()
  # -------------------------------------
  #     tab1=data.frame(tab$long,tab$lat)
  #     Cdistance=sapply(2:nrow(tab1),function(i){distm(tab1[i-1,],tab1[i,])})
  #     
  #     Cdistance[nrow(tab1)]=10
  #     tab=tab[Cdistance<7,]
  #--------------------------------------
  # end.time <- Sys.time()
  # time.taken <- end.time - start.time
  # time.taken
  
  
  
  ll=cbind(tab$long,tab$lat)
  # D=distm(ll,fun =distHaversine )
  
  xy = mercator(ll)#/10000000
  
  dbs=dbscan(x = xy,eps =200,minPts = 240)
  # dbs=optics(x=xy,eps = 200,minPts = 240)
  tab$cluster=dbs$cluster
  
  # table(dbs$cluster)
  a=unique(dbs$cluster)
  tmp2=tab
  
  
  if(length(a)>0)
  {
    #     par(mfrow = c(1,1))
    
    #removing cluster zero (noise)
    # temp=tab[tab$cluster!=0,]
    # temp$ID<-seq.int(nrow(temp))
    
    StayPoints=NULL
#     while(!is.null(temp))
#     {
#       t1<-temp[!duplicated(temp$cluster),]
#       tmp2<-temp
#       if(nrow(t1)==1)
#       {
#         tmp2=temp
#         temp=NULL
#       }
#       else
#       {
#         tmp2=temp[which(temp$ID<t1[2,]$ID),]  
#         temp=temp[temp$ID>=t1[2,]$ID,]
#         
#       }
    for(i in 1:length(a)-1){
      # qq=aggregate(mercator(tmp2[tmp2$cluster==i, 2:1]), list(tmp2$cluster==i), median)
      # StayPoints=as.data.frame(rbind(StayPoints,mercator(qq[,2:3]
      #                         ,inverse = TRUE)))
      qq=mercator(cbind(tmp2[1,]$long,tmp2[1,]$lat))
      Tdiff=as.integer(difftime(units = "m",
                                tmp2[nrow(tmp2),]$date,
                                tmp2[1,]$date))
      if(Tdiff<20)
        next
      distt2=distm(cbind(tmp2[c(1,nrow(tmp2)),]$lon,
                         tmp2[c(1,nrow(tmp2)),]$lat))
      # distt=distm(cbind(tmp2$lon,tmp2$lat))
      
      if(max(distt2)>200)
        next
      if(is.null(StayPoints))
        tempframe=data.frame(mercator(qq,inverse = TRUE),
                             Start_time=tmp2[1,]$date,
                             End_time=tmp2[nrow(tmp2),]$date,
                             Day=tmp2[1,]$day,
                             prev_lon=NA,
                             prev_lat=NA,
                             Duration_m=as.integer(difftime(units = "m",tmp2[nrow(tmp2),]$date,tmp2[1,]$date)))
      else
        tempframe=data.frame(mercator(qq,inverse = TRUE),
                             Start_time=tmp2[1,]$date,
                             End_time=tmp2[nrow(tmp2),]$date,
                             Day=tmp2[1,]$day,
                             prev_lon=StayPoints[nrow(StayPoints),]$lon,
                             prev_lat=StayPoints[nrow(StayPoints),]$lat,
                             Duration_m=as.integer(difftime(units = "m",tmp2[nrow(tmp2),]$date,tmp2[1,]$date)))
      
      StayPoints=as.data.frame(rbind(StayPoints,tempframe))
      
     }
    StayPoints2=as.data.frame(rbind(StayPoints2,StayPoints))
    
    #     x=data.frame(xy,dbs$cluster)
    #     x=x[x$dbs.cluster!=0,]
    #     qq=aggregate(x[, 1:2], list(x$dbs.cluster), mean)
    
    # StayPoints2=as.data.frame(rbind(StayPoints2,mercator(qq[,2:3],inverse = TRUE)))
    
    
    #     plot(ll,sub=f)
    #     points(StayPoints, type = "p",col = "blue", cex = 1,pch = 19)
    
    
    # StayPoints2=NULL
    
    #     plot(ll)
    #     points(geomean(ll), type = "p",col = "blue", cex = 2.5,pch = 19)
  # }
  # 
  # oo=as.data.frame(ll)
  # tt=as.data.frame(mercator(qq[,2:3],inverse = TRUE))
  # map <- get_map(location = ll[1,],zoom=15)
  # x<-ggmap(map)
  # x + geom_point(data = tt, aes(x = tt$lon, y = tt$lat))
  #  
  # qmplot(V1, V2, data =as.data.frame(ll), colour = I("red"), size = I(3), darken = .3 )
  
  
  }
}
