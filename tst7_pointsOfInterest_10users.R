library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)

f="D:/StayPoints.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))


for(q in 
    unique(table_user_training$UserID)
    # 1
)
{
  set.seed(1)
  # q=1
  
 
  table_user=table_user_training[table_user_training$UserID==q,]
  
  
  
  
  # table_user=as.data.frame(rbind(table_user_training,table_user_testing))
  
  table_user$date = as.POSIXct(paste(table_user[,9],table_user[,10]),tz = "GMT") 
  # attributes(table_user$date)$tzone <- "Asia/Shanghai"
  
  table_user$hour=hour(table_user$date)
  table_user$day=weekdays(table_user$date)
  ll=cbind(table_user$Long,table_user$Lat)
  xy = mercator(ll)#/10000000
  
  table_user$x=xy[,1]
  table_user$y=xy[,2]
  
  
  table_user1=table_user#[table_user$day=="Saturday",]
  
  dbnew=dbscan(distm(ll),eps = 100,MinPts = 4,method = "dist")
  
  # plot(table_user$x,table_user$y,col=dbnew$cluster)
  # dbnew=dbscan(table_user1[,c("x","y")] ,eps =100,MinPts = 4)
  
  
  qq=aggregate(table_user1[, c("x","y")], list(dbnew$cluster), median)
  qr=mercator(qq[,2:3],inverse = TRUE)
  
  cols <- rainbow(length(unique(dbnew$cluster)),alpha = NULL)
  # pdf$colors <- cols[unclass(pdf$Study)]
  
  table_user1=as.data.frame(cbind(table_user1,location1=dbnew$cluster))
  
  # dbscan_location=dbscan(table_user1[,c("x","y")] ,eps =100,MinPts = 4)
#   plot(dbscan_location,table_user1[,c("x","y")],Color=dbscan_location$cluster)
#   table(dbscan_location$cluster)
  
#   qq1=aggregate(table_user1[, c("x","y")], list(dbscan_location$cluster), mean)
#   qr1=mercator(qq1[,2:3],inverse = TRUE)
  
  # table_user1=as.data.frame(cbind(table_user1,location=dbscan_location$cluster))

#   gg=cbind(id=rep(q,nrow(qq[-1,2:3])),qq[-1,2:3])
#    histo=as.data.frame(rbind(histo,gg))
  # print(length(unique(dbscan_location$cluster)))
  print("number of StayPoints")
  print(nrow(table_user1))
  print("number of POIs")
  print(length(unique(dbnew$cluster))-1)
  # histo=as.data.frame(rbind(histo,c(nrow(table_user1),length(unique(dbnew$cluster))-1)))
  #plot(histo)
# #   
#   leaflet()%>%
#     addTiles() %>%
#     # addPolylines(data = qr, lng = qr[,1], lat = qr[,2])%>%
#     addMarkers(data = qr, lng = qr[2:nrow(qr),1], lat = qr[2:nrow(qr),2])
# # 
#   leaflet()%>%
#     addTiles() %>%
#     # addPolylines(data = qr, lng = qr[,1], lat = qr[,2])%>%
#     addCircleMarkers(data = qr1, lng = qr1[,1], lat = qr1[,2],radius = 5)

#------------------------------------------------------------------  
#   leaflet(table_user1)%>%
#     addTiles() %>%
#     addMarkers(table_user1, lng=~Long, lat=~Lat,popup =as.character(table_user1$SpID)) #%>%
    # addCircleMarkers(m, lng=qr[,1], lat=qr[,2])
    # addCircleMarkers(radius = 4,table_user1, lng=~Long, lat=~Lat,popup =as.character(table_user1$SpID)) 
  #------------------------------------------------------------------  
  
#     %>%
    # addCircles(data = qr, lng = qr[,1], lat = qr[,2],radius = 50,color = "red")
  
#-----------------------------------------------------
  map = leaflet(table_user1) %>% addTiles()
  for(g in unique(table_user1$location1)){
    d = table_user1[table_user1$location1 == g, ]
    map = map %>% addCircleMarkers(radius = 8,data = d, lng = ~Long, lat = ~Lat, 
                                   color = ~cols[g])
    # map = map %>% addMarkers(data = d, lng = ~Long, lat = ~Lat)
  }
  map %>% addLayersControl(overlayGroups = unique(table_user1$location1))  #%>%
  # addCircles(data = qr, lng = qr[2:nrow(qr),1], lat = qr[2:nrow(qr),2],radius = 50,color = "Brown",opacity = 80)
  
}

# map
  
  #-----------------------------------------------------
  



  