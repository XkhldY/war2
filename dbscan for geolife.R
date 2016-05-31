# library("flexclust")
library("geosphere")
library("ggmap")
library("ggplot2")
library("fossil")
library(LPCM)
library(dbscan)
library("lubridate")

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_2.txt"
tab=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))
    
     tab$date = as.POSIXct(paste(tab[,8],tab[,9]),tz = "GMT") 
     attributes(tab$date)$tzone <- "Asia/Shanghai"
     tab$hour=hour(tab$date)

# # start.time <- Sys.time()
# # end.time <- Sys.time()
# # time.taken <- end.time - start.time
# # time.taken
 
ll=cbind(tab$Long,tab$Lat)
xy = mercator(ll)#/10000000

tab$x=xy[,1]
tab$y=xy[,2]
plot(xy)

# mydata$Agegroup2<-ifelse(tab$hour<6,,     
#                          ifelse(mydata$Age>20, 2,0))

dbs=dbscan(tab[,c("x","y")] ,eps =15,minPts = 1)

#clustering Staypoints with  4 independent periods of time 
db1=dbscan(tab[tab$hour<6,c("x","y")] ,eps =1000,minPts = 5)
db2=dbscan(tab[tab$hour>=6 & tab$hour<12,c("x","y")] ,eps =100,minPts = 1)
db3=dbscan(tab[tab$hour>=12 & tab$hour<18,c("x","y")] ,eps =100,minPts = 1)
db4=dbscan(tab[tab$hour>=18,c("x","y")] ,eps =100,minPts = 1)

#clustered staypoints
tab1=as.data.frame(cbind(tab[tab$hour<6,],cluster=db1$cluster))
tab2=as.data.frame(cbind(tab[tab$hour>=6 & tab$hour<12,],cluster=db2$cluster))
tab3=as.data.frame(cbind(tab[tab$hour>=12 & tab$hour<18,],cluster=db3$cluster))
tab4=as.data.frame(cbind(tab[tab$hour>=18,],cluster=db4$cluster))
# dbs=optics(x=xy,eps = 200,minPts = 240)
tab=as.data.frame(cbind(tab,cluster=dbs$cluster))
# tab$cluster=dbs$cluster

#mean points of clustered staypoints
qq=aggregate(x = tab[,c("x","y")],list(tab$cluster),mean)
# qq
# q=dist(qq)
qq_mod=dbscan(qq,eps = 100,minPts = 2)
plot(xy,col=dbs$cluster)
plot(tab[tab$hour<6,c("Long","Lat")])

qq1=as.data.frame(mercator(qq[,c(2,3)],inverse = TRUE))
q=distm(qq1)
qq2=mercator(qq[,c(2,3)],inverse = TRUE)

# qq1$cluster=dbs$cluster


# -----------------------------------------------
library(leaflet)
m <- leaflet()
m <- addTiles(m)
# m <- addMarkers(m, lng=tab[tab$UserID==2,"Long"], lat=tab[tab$UserID==2,"Lat"])
 m <- addCircleMarkers(m, lng=qq1$lon, lat=qq1$lat,color = dbs$cluster)
 
#  (radius = ~ifelse(type == "ship", 6, 10),color = ~pal(type),stroke = FALSE, fillOpacity = 0.5)
# m <- addMarkers(m, qq2[,1],qq2[,2],popup = "stay")
# m <- addCircles(qq2[,1], qq2[,2], radius = runif(50, 50, 150))
m

leaflet(qq2) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)
#---------------------------------------------------
  
# qmplot(Long, Lat, data =tab[tab$UserID==1,], 
#        colour = I("red"), size = I(3), darken = .3 )

# # a=unique(dbs$cluster)
# #   
# #   if(length(a)>1)
# #   {
# # #     par(mfrow = c(1,1))
# #     
# #     #removing cluster zero (noise)
# #     temp=tab[tab$cluster!=0,]
# #     temp$ID<-seq.int(nrow(temp))
# #     
# #     StayPoints=NULL
# #     while(!is.null(temp))
# #     {
# #       t1<-temp[!duplicated(temp$cluster),]
# #       if(nrow(t1)==1)
# #       {
# #         tmp2=temp
# #         temp=NULL
# #       }
# #       else
# #       {
# #         tmp2=temp[which(temp$ID<t1[2,]$ID),]  
# #         temp=temp[temp$ID>=t1[2,]$ID,]
# #         
# #       }
# #      
# #       qq=aggregate(mercator(tmp2[, 2:1]), list(tmp2$cluster), median)
# #       # StayPoints=as.data.frame(rbind(StayPoints,mercator(qq[,2:3]
# #       #                         ,inverse = TRUE)))
# #       Tdiff=as.integer(difftime(units = "m",
# #                                 tmp2[nrow(tmp2),]$date,
# #                                 tmp2[1,]$date))
# #       if(Tdiff<20)
# #         next
# #       distt2=distm(cbind(tmp2[c(1,nrow(tmp2)),]$lon,
# #                          tmp2[c(1,nrow(tmp2)),]$lat))
# #       # distt=distm(cbind(tmp2$lon,tmp2$lat))
# #       
# #       if(max(distt2)>200)
# #         next
# #       if(is.null(StayPoints))
# #       tempframe=data.frame(mercator(qq[,2:3],inverse = TRUE),
# #                            Start_time=tmp2[1,]$date,
# #                            End_time=tmp2[nrow(tmp2),]$date,
# #                            Day=tmp2[1,]$day,
# #                            prev_lon=NA,
# #                            prev_lat=NA,
# #                            Duration_m=as.integer(difftime(units = "m",tmp2[nrow(tmp2),]$date,tmp2[1,]$date)))
# #       else
# #         tempframe=data.frame(mercator(qq[,2:3],inverse = TRUE),
# #                              Start_time=tmp2[1,]$date,
# #                              End_time=tmp2[nrow(tmp2),]$date,
# #                              Day=tmp2[1,]$day,
# #                              prev_lon=StayPoints[nrow(StayPoints),]$lon,
# #                              prev_lat=StayPoints[nrow(StayPoints),]$lat,
# #                              Duration_m=as.integer(difftime(units = "m",tmp2[nrow(tmp2),]$date,tmp2[1,]$date)))
# #       
# #       StayPoints=as.data.frame(rbind(StayPoints,tempframe))
# #       
# #     }
# #     StayPoints2=as.data.frame(rbind(StayPoints2,StayPoints))
# #     
# #     x=data.frame(xy,dbs$cluster)
# #     x=x[x$dbs.cluster!=0,]
# #     qq=aggregate(x[, 1:2], list(x$dbs.cluster), mean)
#     
#     # StayPoints2=as.data.frame(rbind(StayPoints2,mercator(qq[,2:3],inverse = TRUE)))
#     
#     
# #     plot(ll,sub=f)
# #     points(StayPoints, type = "p",col = "blue", cex = 1,pch = 19)
#     
#     
#     # StayPoints2=NULL
#     
# #     plot(ll)
# #     points(geomean(ll), type = "p",col = "blue", cex = 2.5,pch = 19)
#   }
# 
# oo=as.data.frame(ll)


# }
