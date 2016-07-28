require("ggmap")
require(data.table)
require(plyr)
require(lubridate)
library(leaflet)
require(foreach)
library(Rcpp)
library(doParallel)
# library("fpc")
library(dbscan)
library("geosphere")

# temp <- tempfile()
# # download.file("https://snap.stanford.edu/data/loc-gowalla_totalCheckins.txt.gz",temp)
# data<-read.table(gzfile("D:\\work\\Research\\location research\\datasets\\loc-gowalla_totalCheckins.txt.gz"))  
# png("Gowalla_Bilbao.png",height=1280,width=1080)
data <- fread("D:\\All_StayPoints.txt")
colnames(data)[which(names(data) == "UserID")] <- "user"
data$`check-in time`=paste(data$date,data$time)
colnames(data)[which(names(data) == "Long")] <- "longitude"
colnames(data)[which(names(data) == "Lat")] <- "latitude"


ll=cbind(data$longitude,data$latitude)
xy = mercator(ll)#/10000000

dbnew=dbscan(xy,eps = 50,minPts = 1)

data$location_id=dbnew$cluster
# new.names <- c("user","check-in time", "latitude", "longitude",	"location_id")
# names(data) <- new.names
data$`check-in time`<-parse_date_time(data$`check-in time`,"YmdHMS")

# newdata <- data[order(data$`check-in time`, data$user),] 
# newdata <- data[with(data, order(`check-in time`, user)),]

newdata<-ddply(data, .(user), function(x) x[with(x, order(`check-in time`)),])
# str(newdata)
# x=NULL
data_50more<-ddply(newdata, .(user), function(x) nrow(x)>50)

data_50more<-data_50more[data_50more$V1==TRUE,]
data_50more<-newdata[newdata$user%in%data_50more$user,]
data_50more$label<-1

set.seed(1)
training_set<-ddply(data_50more,.(user),function(x) x[1:(floor(.7*nrow(x))),])
testing_set<-ddply(data_50more,.(user),function(x) x[(floor(.7*nrow(x))+1):nrow(x),])

train_temp=training_set

training_set=testing_set
#-----------------------Rcpp-------adding novel and regularity labels------
registerDoParallel()
getDoParWorkers()
system.time(training_set_label<-ddply(training_set,.(user),adding_labels))

#-------------------------features-----------------
#-----temporal features
#-----(1)------hour of day
training_set_label$HOD<-hour(training_set_label$`check-in time`)
# training_set_label$DOW<-weekdays


#-----(2)------day of the week coded 0-6
training_set_label$DOW=weekdays(training_set_label$`check-in time`)
a<-c("Saturday"="0","Sunday"="1","Monday"="2","Tuesday"="3","Wednesday"="4","Thursday"="5","Friday"="6")
training_set_label$DOW<-a[training_set_label$DOW]

#-----(3)------hour of the week---------------
training_set_label$HOW<-training_set_label$HOD+24*as.integer(training_set_label$DOW)
#-----(4)------time difference between consective points in hours--------
# system.time(
#   g<-foreach(n = sort(unique(training_set_label$user)),.combine = rbind) %:% 
#     foreach(m =1:nrow(training_set_label[training_set_label$user==n,]),.combine = rbind) %dopar% 
#     difftime(training_set_label[training_set_label$user==n,]$`check-in time`[m],training_set_label[training_set_label$user==n,]$`check-in time`[m-1],units = c("hours"))
# )

difft<-function(x)
{
  diff=difftime(x$`check-in time`[2:nrow(x)],
                x$`check-in time`[1:(nrow(x)-1)],units = "hours" )
  diff=c(0,diff)  
  x$timediff=diff
  return(x)
}

training_set_label<-ddply(.data =training_set_label,.(user),difft)



# system.time(
#   g<-foreach(n = sort(unique(training_set_label$user)),.combine = rbind) %:% 
#   foreach(m =1:nrow(training_set_label[training_set_label$user==n,]),.combine = rbind) %dopar% 
#   difftime(training_set_label[training_set_label$user==n,]$`check-in time`[m],training_set_label[training_set_label$user==n,]$`check-in time`[m-1],units = c("hours"))
# )

#------visiting ratio------------------
# ll=cbind(training_set_label$longitude,training_set_label$latitude)
# xy = mercator(ll)#/10000000
# system.time(dis<-dist(xy))
# 
# system.time(dbnew<-dbscan(dis,eps = 3000,MinPts = 1,method = "dist"))
# 
# x1$cluster=dbnew$cluster

# system.time(dbnew2<-dbscan(dis,eps = 3000,minPts = 1))

#-----spatial features

#----(5)------visiting ratio-----------

training_set_label=subset(training_set_label,latitude<90)

system.time(data_one_loc_id<-training_set_label[!duplicated(training_set_label$location_id), ])
ll=cbind(data_one_loc_id$longitude,data_one_loc_id$latitude)
xy = mercator(ll)#/10000000

system.time(dbnew<-dbscan(x = xy,eps = 3000,minPts = 1))

data_one_loc_id$cluster=dbnew$cluster
training_set_label$cluster=0

a=data_one_loc_id[order(data_one_loc_id$location_id),]$cluster
b=training_set_label[order(training_set_label$location_id),]$location_id
c=as.factor(b)
d=a[c]
training_set_label$cluster=d

# fun<-function(x,data_one_loc_id) 
# {
#   # print(x$location_id)
#   x$cluster=data_one_loc_id[which(data_one_loc_id$location_id==x[1,]$location_id),]$cluster
# }

# ddply(training_set_label, "location_id",function(x,y) fun(x,y),data_one_loc_id)
# q=training_set_label[training_set_label$user==0,]

#can we find an alternative
# system.time(training_set_label<-ddply(training_set_label, "location_id",function(x,y) addclust(x,y),data_one_loc_id))

# training_set_label<-ddply(training_set_label, .variables = c("user"),function(x,y,z) VisitingRatio(x,y,z),training_set_label$cluster,unique(training_set_label$cluster))

training_set_label$VisitingRatio=0

VisitingRatioR<-function(q,cluster)
{
  for(i in 1:nrow(q))
  {
    count1=length(q[q$cluster==q[i,]$cluster,]$cluster)
    count2=length(cluster[cluster==q[i,]$cluster])
    
    q[i,]$VisitingRatio=count1/count2
    
  }
  return(q)
}
# a[7577][[1]]
visitR<-function(q,clustertable)
{
  co=unique(q$cluster)
  print(q[1,]$user)
  for(i in co)
  {
    a=q[q$cluster==i,]
    b=nrow(a)
    q[q$cluster==i,]$VisitingRatio=b/clustertable[i][[1]]
  }
  return(q)
}

#too slow
# system.time(training_set_label<-ddply(training_set_label, .variables = c("user"),function(x,y) VisitingRatioR(x,y),training_set_label$cluster))

clustertable<-table(training_set_label$cluster)
system.time(training_set_label<-ddply(training_set_label, .variables = c("user"),function(x,y) visitR(x,y),clustertable))


#------(6)------avg difference in distance  with the previous location--------

distt<-function(x)
{
  p1=cbind(x[1:(nrow(x)-1),]$longitude,x[1:(nrow(x)-1),]$latitude)
  p2=cbind(x[2:(nrow(x)),]$longitude,x[2:(nrow(x)),]$latitude)
  ddist=distHaversine(p1,p2)
  ddist=c(0,ddist)  
  x$distdiff=ddist/1000
  return(x)
}

system.time(training_set_label<-ddply(.data =training_set_label,.(user),distt))
#--------(7) location Entropy-------------


#-----Historical features
#--------(8) Distinct no. of locations-------------
distinctloc<-function(x)
{
  x$distinct=length(unique(x$location_id))
  return(x)
}
system.time(training_set_label<-ddply(.data =training_set_label,.(user),distinctloc))

#--------(9) User Entropy-------------
#--------(10) Novel Ratio-------------

# qplot(x =1:nrow(data_50more_wlabels_Nratio),geom=c("point", "smooth"),y =Nratio,data = data_50more_wlabels_Nratio)

AddingID<-function(x){
  x$row_id=seq(1,nrow(x))
  return(x)
  
} 
training_set_label<-ddply(training_set_label,.(user),AddingID)



CalcNovelityRatio<-function(x)
{
  x$Nratio=0
  print(unique(x$user))
  # data=x
  
  # x=lapply(x, function(x, y) func11(x,y), y=data)
  for(i in 1:nrow(x))
  {
    x[i,]$Nratio=nrow(x[x$label==1 & x$row_id<=i,])/nrow(x[x$row_id<=i,])
  }
  return(x)
}
#need to be more smart
training_set_label<-ddply(training_set_label,.(user),CalcNovelityRatio)
#--------(11) No. of Days-------------

# table_user1$UserID)

UniqueDays<-function(x)
{
  lis=aggregate(x[,"location_id"],list(year(x$`check-in time`),
                                       day(x$`check-in time`),
                                       month(x$`check-in time`)),list)
  x$NoOfDays=nrow(lis)
  return(x)
}

system.time(training_set_label<-ddply(.data =training_set_label,.(user),UniqueDays))

#--------(12) novelity of previous check-in-------------
# q=training_set_label[training_set_label$user<1000,]
# system.time(q<-ddply(.data =q,.(user),PreNovelityCheck))
#another sol. more speedy :D
system.time(training_set_label<-ddply(.data =training_set_label,.(user),
                                      function(x){
                                        preNovel=shift(x$label,fill=0)
                                        x$preNovel=preNovel
                                        return(x)
                                      }))

#-------next location novel or regular--------------------
system.time(training_set_label<-ddply(.data =training_set_label,.(user),
                                      function(x){
                                        NextNovel=shift(x$label,type = "lead",fill=0)
                                        x$NextNovel=NextNovel
                                        return(x)
                                      }))

#---------------add to file------------------------
write.csv(training_set_label,"d:/testing_geolife2.csv",row.names = FALSE)
# #---------------------------------------models
# training_set_label$label<-as.numeric(training_set_label$label)
d<-glm(formula = label ~ preNovel+
         NoOfDays+
         Nratio+
         distinct+
         distdiff+
         VisitingRatio+
         timediff+
         HOW+
         DOW+
         HOD, 
       family = binomial, data = training_set_label)

library(caret)
varImp(d)

# 
# sapply(training_set_label,sd)
# 
# xtabs(label ~ 
# #         preNovel
# #       +
#         # NoOfDays
# #       +
#         # Nratio
# #       +
#         distinct
# #       +
# #         distdiff
# #       +
# #         VisitingRatio
# #       +
# #         timediff
# #       +
# #         HOW
# #       +
# #         DOW
# #       +
# #         HOD
#       ,data = training_set_label)
#"Saturday"="0","Sunday"="1","Monday"="2","Tuesday"="3","Wednesday"="4","Thursday"="5","Friday"="6"

#---way too slow
# foreach(n = sort(unique(data_50moreR$user))) %do% tst2(data_50moreR[data_50moreR$user==n,])
#----------------------R
# system.time(data_50moreR<-ddply(data_50moreR,.(user),func))
# table(data_50moreR$label)
# table(data_50moreR[data_50moreR$user==0,]$label)

# 
# for(i in unique(data_50more$user)){
#   print(
#   cbind(table(data_50moreC[data_50moreC$user==i,]$label),
#      table(data_50moreR[data_50moreR$user==i,]$label)))
#   }


#------------------2nd iteration------

# func<-function(x)
# {
#   x=data.table(x)
#   z<-x[1,]$location_id
#   print(unique(x$user))
#   for(i in 2:nrow(x))
#   {
#     if(x[i,]$location_id %in% z)
#     {
#       x[i,]$label<-0
#       
#     }
#     z=rbind(z,x[i,]$location_id)
#   }
#   
#   
#   return (x)
# }

#---------------------------- interesting but slow
# func33<-function(x,z)
# {
#     print(x$user)
#     if(x$location_id %in% z)
#       x$label<-0
#   return (x)
# }
# foreach(n = sort(unique(data_50moreR$user))) %:% 
#   foreach(m =1:nrow(data_50moreR[data_50moreR$user==n,])) %do% 
#   func33(data_50moreR[data_50moreR$user==n,][m],data_50moreR[data_50moreR$user==n,][1:m]$location_id)
#----------------------------

# q=factor(x$label)
# qplot(q) 
# summary(q)

# data_50more$label=1


#-------------------getting returning time diffecene between 
#-------------------first visit to a location and its next one.
# func7<-function(x)
# {
#   data_1_label_0<-x[x$label==0,]
#   data_1_label_1<-x[x$label==1,]
#   if(nrow(data_1_label_0)<1 | nrow(data_1_label_1)<1)
#   {
#     data_1_label_0_1st=data_1_label_0
#     data_1_label_0_1st$diff1=0
#     print("error")
#     print(unique(x$user))
#     
#     return(data_1_label_0_1st)
#     
#   }
#   
#   data_1_label_0_1st<-ddply(data_1_label_0, "location_id", head, 1)
#   print(unique(x$user))
#   data_1_label_0_1st$diff1=0
#   for(i in 1:nrow(data_1_label_0_1st))
#   {
#     data_1_label_0_1st[i,]$diff1<- difftime(data_1_label_1[data_1_label_1$location_id ==
#                                                              data_1_label_0_1st[i,]$location_id,]$`check-in time`,
#                                             data_1_label_0_1st[i,]$`check-in time`)
#     # data_1_label_1[which(data_1_label_1$location_id == data_1_label_0_1st[i,]$location_id), ]$`check-in_time`
#   }
#   
#   return (data_1_label_0_1st)
# }

# data_50more_wlabels_diff=ddply(training_set_label,.(user),func7)

# data_50more_wlabels_diff$diff1
