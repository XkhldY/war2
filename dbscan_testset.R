# library("flexclust")
library("geosphere")
library("ggmap")
library("ggplot2")
library("fossil")
library(LPCM)
library(dbscan)
library("lubridate")
library(markovchain) 

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_training_4.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_testing_4.txt"
table_user_testing=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))

table_user=as.data.frame(rbind(table_user_training,table_user_testing))



table_user$date = as.POSIXct(paste(table_user[,8],table_user[,9]),tz = "GMT") 
# attributes(table_user$date)$tzone <- "Asia/Shanghai"
table_user$hour=hour(table_user$date)
table_user$day=weekdays(table_user$date)
ll=cbind(table_user$Long,table_user$Lat)
xy = mercator(ll)#/10000000

table_user$x=xy[,1]
table_user$y=xy[,2]
# plot(xy)


dbscan_location=dbscan(table_user[,c("x","y")] ,eps =15,minPts = 1)


table_user=as.data.frame(cbind(table_user,location=dbscan_location$cluster))


dbscan_grid=dbscan(table_user[,c("x","y")] ,eps =200,minPts = 1)

table_user=as.data.frame(cbind(table_user,gridcell=dbscan_grid$cluster))

table_user_training=table_user[1:nrow(table_user_training),]
table_user_testing=table_user[(nrow(table_user_training)+1):nrow(table_user),]

# Qij=nrow(table_user[table_user[i,"gridcell"]==i & table_user[j,"gridcell"]==i,])

# TestSamples=sample(table_user[table_user$prev_SpID!=-1,]$prev_SpID,
#                    size = length(table_user$prev_SpID)*.3,replace = FALSE)


#loop to get sequences per day i.e. everyday is a new sequence
i=1
Sequences_test=NULL
while(i<nrow(table_user_testing))
{
  sequence=as.character(table_user_testing[i,]$location)
  k=i
  i=i+1
  while(table_user_testing[i,]$prev_SpID!=-1 & i<=nrow(table_user_testing))
  {
    # sequence=c(sequence,table_user[i,]$location)
    sequence=stringi::stri_join(sequence,as.character(table_user_testing[i,]$location),sep=",")
    i=i+1
  }
  Sequences_test=as.data.frame(rbind(Sequences_test,c(sequence,table_user_testing[k,]$day)),stringsAsFactors=FALSE)
}

# TestSamples=sample(nrow(Sequences),size = nrow(Sequences)*.3,replace = FALSE)


# TrainingSet=table_user

emission=matrix(0,nrow = length(unique(table_user$gridcell))
                ,ncol = length(unique(table_user$location)))

# loop to get the emission matrix
for(i in 1:length(unique(table_user$gridcell)))
  for(j in 1:length(unique(table_user$location)))
  {
    emission[i,j]=nrow(table_user[table_user$location==j & table_user$gridcell==i,])/nrow(table_user[table_user$gridcell==i,])
  }

# using markovchain package to get the transition matix
State_seq<- table_user$gridcell
mcFit <- markovchainFit(data=State_seq)

write.csv(x = mcFit$estimate@transitionMatrix,file = "D:\\work\\Research\\location research\\datasets\\Geolife\\StateTransitions.csv")
write.csv(x = emission,file = "D:\\work\\Research\\location research\\datasets\\Geolife\\Emissions.csv")
write.table(x = Sequences,file = "D:\\work\\Research\\location research\\datasets\\Geolife\\sequences.csv",row.names = FALSE,sep = ";")

# tst_transition=head(table_user,n = 20)
# z=NULL
# for(i in 1:nrow(tst_transition))
# {
#   Xi=length(tst_transition[tst_transition$gridcell==i,])
#   for(j in 1:nrow(tst_transition))
#   {
#     tst_transition[tst_transition$gridcell==j,]
#     tst_transition[tst_transition$gridcell==j,"SpID"]-1
#     
#     Xj_Xi=length(tst_transition[tst_transition$SpID==tst_transition[tst_transition$gridcell==j,"SpID"]-1,])
#   }
# }



#clustering Staypoints with  4 independent periods of time 
# db1=dbscan(table_user[table_user$hour<6,c("x","y")] ,eps =15,minPts = 1)
# db2=dbscan(table_user[table_user$hour>=6 & table_user$hour<12,c("x","y")] ,eps =15,minPts = 1)
# db3=dbscan(table_user[table_user$hour>=12 & table_user$hour<18,c("x","y")] ,eps =15,minPts = 1)
# db4=dbscan(table_user[table_user$hour>=18,c("x","y")] ,eps =15,minPts = 1)
# 
# #clustered staypoints
# table_user1=as.data.frame(cbind(table_user[table_user$hour<6,],cluster=db1$cluster))
# table_user2=as.data.frame(cbind(table_user[table_user$hour>=6 & table_user$hour<12,],cluster=db2$cluster))
# table_user3=as.data.frame(cbind(table_user[table_user$hour>=12 & table_user$hour<18,],cluster=db3$cluster))
# table_user4=as.data.frame(cbind(table_user[table_user$hour>=18,],cluster=db4$cluster))
# dbs=optics(x=xy,eps = 200,minPts = 240)

# table_user$cluster=dbs$cluster

#mean points of clustered staypoints
qq=aggregate(x = table_user[,c("x","y")],list(table_user$cluster),mean)
# qq
# q=dist(qq)
qq_mod=dbscan(qq,eps = 100,minPts = 2)
# plot(xy,col=dbs$cluster)
# plot(table_user[table_user$hour<6,c("Long","Lat")])

qq1=as.data.frame(mercator(qq[,c(2,3)],inverse = TRUE))
# q=distm(qq1)
qq2=mercator(qq[,c(2,3)],inverse = TRUE)

# qq1$cluster=dbs$cluster


# -----------------------------------------------
library(leaflet)
m <- leaflet()
m <- addTiles(m)
# m <- addMarkers(m, lng=table_user[table_user$UserID==2,"Long"], lat=table_user[table_user$UserID==2,"Lat"])
m <- addCircleMarkers(m, lng=qq1$lon, lat=qq1$lat,color = dbs$cluster)

#  (radius = ~ifelse(type == "ship", 6, 10),color = ~pal(type),stroke = FALSE, fillOpacity = 0.5)
# m <- addMarkers(m, qq2[,1],qq2[,2],popup = "stay")
# m <- addCircles(qq2[,1], qq2[,2], radius = runif(50, 50, 150))
m

leaflet(qq2) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

#---------------------------------------------------
sequence=c()
for(i in 1:nrow(table_user))
{
  
  if(table_user[i,]$prev_SpID==-1)
    sequence=c(sequence,"START",table_user[i,]$cluster)
  else if(i==nrow(table_user))
    sequence=c(sequence,table_user[i,]$cluster,"END")
  else if(table_user[i+1,]$prev_SpID==-1)
    sequence=c(sequence,table_user[i,]$cluster,"END")
  else
    sequence=c(sequence,table_user[i,]$cluster)
}
mcFit<-markovchainFit(data=sequence)

# trainMc<-function(sequences){
#   sequence<-c()
#   for (i in 1:length(sequences)){
#     sequence<-c(sequence,"START",unlist(sequences[i]),"END")
#   }
# 
#   Mc<-as(mcFit$estimate, "markovchain")
#   return(Mc)
# }