require("ggmap")
require(data.table)
require(plyr)
require(lubridate)
library(leaflet)
require(foreach)


# temp <- tempfile()
# # download.file("https://snap.stanford.edu/data/loc-gowalla_totalCheckins.txt.gz",temp)
# data<-read.table(gzfile("D:\\work\\Research\\location research\\datasets\\loc-gowalla_totalCheckins.txt.gz"))  
# png("Gowalla_Bilbao.png",height=1280,width=1080)
data <- fread("D:\\work\\Research\\location research\\datasets\\Gowalla_totalCheckins.txt")
new.names <- c("user","check-in time", "latitude", "longitude",	"location_id")
names(data) <- new.names
data$`check-in time`<-parse_date_time2(data$`check-in time`,"YmdHMS")

newdata <- data[order(`check-in time`),] 

str(newdata)
# x=NULL


data_50more<-ddply(newdata, .(user), function(x) nrow(x)>50)

data_50more<-data_50more[data_50more$V1==TRUE,]
data_50more<-newdata[newdata$user%in%data_50more$user,]
data_50more$label<-1

# ?ddply

# ddply(data_50more[data_50more$user%in%(1:10),], .(user), function(x) length(x$location_id))



func<-function(x)
{
  x=data.table(x)
  z<-x[1,]$location_id
  print(unique(x$user))
  for(i in 2:nrow(x))
  {
    if(x[i,]$location_id %in% z)
      x[i,]$label<-0
    else
      z=rbind(z,x[i,]$location_id)
  }
  return (x)
}

# Y=ddply(data_50more[data_50more$user<1,],.(user),func3)
# Y=ddply(data_50more,.(user),func3)
# q=factor(Y$label)
# qplot(q) 
# summary(q)
# func3<-function(A)
# {
#   z<-A[1,]$location_id
#   G=foreach(i = 1:nrow(A),.combine = rbind) %do% func2(A[i,],z)
#   # print(z)
#   # z<-rbind(z,A$location_id)
#   return (G)
# }
# 
# func2<-function(x,z)
# {
#     if(x[i,]$location_id %in% z)
#       x[i,]$label<-0
#     else
#      {
#        z=rbind(z,x[i,]$location_id)
#        # print(z)
#      }
#     
#     return (x)
# }


x=ddply(data_50more[data_50more$user==0,],.(user),func)
q=factor(x$label)
qplot(q) 
summary(q)
#---------------------------------------------------------------------------

func7<-function(x)
{
  data_1_label_0<-x[x$label==0,]
  data_1_label_1<-x[x$label==1,]
  if(nrow(data_1_label_0)<1 | nrow(data_1_label_1)<1)
  {
    data_1_label_0_1st$diff1=0
    print("error")
    print(unique(x$user))
    
    return(data_1_label_0_1st)
    
  }
  data_1_label_0_1st<-ddply(data_1_label_0, "location_id", head, 1)
  print(unique(x$user))
  data_1_label_0_1st$diff1=0
  for(i in 1:nrow(data_1_label_0_1st))
  {
    data_1_label_0_1st[i,]$diff1<- difftime(data_1_label_1[data_1_label_1$location_id ==
                                                             data_1_label_0_1st[i,]$location_id,]$`check-in_time`,
                                            data_1_label_0_1st[i,]$`check-in_time`)
    # data_1_label_1[which(data_1_label_1$location_id == data_1_label_0_1st[i,]$location_id), ]$`check-in_time`
  }
  return (data_1_label_0_1st)
}

#---------------------------------------------------------------------------
data_50more_wlabels_diff=ddply(x,.(user),func7)

#check the returing time mod the week days
qplot(factor(floor(data_50more_wlabels_diff$diff1)%%7))
#----
data_50more_diff<-subset(data_50more_wlabels_diff,diff1<8)
#----
qplot(factor(floor(data_50more_diff$diff1)))
#----
ggplot(data_50more_diff, aes(x=diff1)) + geom_density()

#---------------------------------------------------------------------------
func10<-function(x)
{
  x$Nratio=0
  print(unique(x$user))
  # data=x
  
  # x=lapply(x, function(x, y) func11(x,y), y=data)
  for(i in 1:nrow(x))
  {
    
    # x[i,]$Nratio=nrow(x[x$label==1 & x$row_id<=i,])/nrow(x[x$row_id<=i,])
    x[i,]$Nratio=(nrow(x)-nrow(x[x$label==0 & x$row_id<i,]))/nrow(x)
  }
  return(x)
}

#---------------------------------------------------------------------------


data_50more_wlabels_Nratio=ddply(data_50more_wlabels[data_50more_wlabels$user<1000,],.(user),func10)
# data_50more_wlabels_Nratio1=data_50more_wlabels_Nratio[with(data_50more_wlabels_Nratio,order("Nratio")),]

x=sort(data_50more_wlabels_Nratio$Nratio,decreasing = TRUE)
qplot(x =1:nrow(data_50more_wlabels_Nratio),geom=c("point", "smooth"),y =x)


# library(doSNOW)
# library(rbenchmark)
#  cluster = makeCluster(4, type = "SOCK")
#  registerDoSNOW(cluster)
# 
#    benchmark(
#      x=foreach(n= 0:1, .combine = c) %:% foreach(.combine = c,m = 2:10) %do% func6(n, m),
#      y=foreach(n= 0:1, .combine = c) %:% foreach(.combine = c,m = 2:10) %dopar% func6(n, m)
#     )
#    stopCluster(cluster)
#  x=foreach(n= 0:0, .combine = c) %:% foreach(.combine = c,m = 2:(nrow(data_50more[data_50more$user==n,]))) %do% func5(n, m,data_50more,z=rbind(z,data_50more[data_50more$user==n,]$location_id))
#  # x=ddply(data_50more[data_50more$user<1,],.(user),func)
#  q=factor(x)
#  qplot(q) 
#  summary(q)
#  func6<-function(j,i)
#  {
#    return(j*i)
#  }
# func5<-function(j,i,data_50more,z)
# {
#   data_tst<-data_50more[data_50more$user==j,]
# #     z=0
# #     if(nrow(data_tst)<1)
# #       next
#     # z<-data_tst[1,location_id]
# #     
# # 
#       if(data_tst[i,]$location_id %in% z)
#         data_tst[i,]$label<-0
#       else
#         z=rbind(z,data_tst[i,]$location_id)
# # 
# #     data_50more[data_50more$user==j,]$label=data_tst$label
#     return(data_50more[data_50more$user==j,][i]$label)
# }


# for(j in 0:10)#data_50more[data_50more$user%in%(1:10),])
# {
#   data_tst<-data_50more[data_50more$user==j,]
#   z=0
#   if(nrow(data_tst)<1)
#     next
#   z<-data_tst[1,location_id]
#   
#   for(i in 2:nrow(data_tst))
#   {
#     if(data_tst[i,]$location_id %in% z)
#       data_tst[i,]$label<-0
#     else
#       z=rbind(z,data_tst[i,]$location_id)
#   }
#   data_50more[data_50more$user==j,]$label=data_tst$label
#   
# }
# 
# ##---------plot novel vs regular locations
# g=data_50more[data_50more$user<11,]$label
# b=factor(g)
# qplot(b) 
# summary(b)
#-----------------------------------------
#please avoid for loops 
# for(i in unique(data$user)[1:10])
# {
#   df=data[data$user==i,]
#   x[i]=difftime(df[1]$`check-in time`,df[nrow(df)]$`check-in time`)
#   
# }
#-------------------------------------------------------------------
#top 100 users spent time in the dataset
#i love speed 
# timedif=ddply(data, .(user), function(x) 
#                             difftime(x[1,]$`check-in time`,x[nrow(x),]$`check-in time`))
# 
# timedif1<-timedif[timedif$V1>500,]
# timedif2<-timedif[timedif$V1<365,]
# checkin_count<-ddply(data,.(user),nrow)
# checkin_count<-checkin_count[checkin_count$V1>100,]
# x=factor(data$location_id,levels = data$user)
# # 
# data1<-data[data$user %in% timedif1$user,]
# data2<-data[data$user %in% checkin_count$user,]
# data3<-data2[data2$user %in% timedif2$user, ]
# qplot(x) + 
#   stat_bin(binwidth = 0.1) +
#   ggtitle("Distribution of the average movie rating")
#-------------------------------------------------------------------

# timedif[timedif$V1>,]
# sort(m$,decreasing = TRUE)[1:100]
# 
# v<-data[order(data$),]
################

# dev.new()
# df <- data[user==11,]# .(x=longitude, y=latitude)]
# # map <- get_map("Bilbao", zoom=14, maptype="toner",source="stamen")
# # 
# # g <- ggmap(map)
# # g <- g+stat_density2d(aes(x = x, y = y, fill=..level..), data=df[1:100],geom="polygon", alpha=0.2)
# # g + scale_fill_gradient(low = "yellow", high = "red")
# # 
# # dev.off()
# 
# m <- leaflet()
# m <- addTiles(m)
# # m <- addMarkers(m, lng=df[1:1000]$x, lat=df[1:1000]$y)
# m <- addCircleMarkers(m, lng=df$longitude, lat=df$latitude)
# 
# m
# hist(islands)
# 
# hist(table(data$user))

