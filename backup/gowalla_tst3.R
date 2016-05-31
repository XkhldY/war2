require("ggmap")
require(data.table)
require(plyr)
require(lubridate)
library(leaflet)
require(foreach)
library(Rcpp)
library(doParallel)

# temp <- tempfile()
# # download.file("https://snap.stanford.edu/data/loc-gowalla_totalCheckins.txt.gz",temp)
# data<-read.table(gzfile("D:\\work\\Research\\location research\\datasets\\loc-gowalla_totalCheckins.txt.gz"))  
# png("Gowalla_Bilbao.png",height=1280,width=1080)
data <- fread("D:\\work\\Research\\location research\\datasets\\Gowalla_totalCheckins.txt")
new.names <- c("user","check-in time", "latitude", "longitude",	"location_id")
names(data) <- new.names
data$`check-in time`<-parse_date_time(data$`check-in time`,"YmdHMS")

# newdata <- data[order(data$`check-in time`, data$user),] 
# newdata <- data[with(data, order(`check-in time`, user)),]

newdata<-ddply(data, .(user), function(x) x[with(x, order(`check-in time`)),])


str(newdata)
# x=NULL


data_50more<-ddply(newdata, .(user), function(x) nrow(x)>50)

data_50more<-data_50more[data_50more$V1==TRUE,]
data_50more<-newdata[newdata$user%in%data_50more$user,]
data_50more$label<-1

data_50moreC=data_50more#[data_50more$user==9,]
data_50moreR=data_50more#[data_50more$user==9,]
#----------------------Rcpp
system.time(m<-tst(data_50moreC))
data_50moreC$label=m
table(data_50moreC$label)
table(data_50moreC[data_50moreC$user==0,]$label)
#-----------------------Rcpp+foreach
registerDoParallel()
getDoParWorkers()
system.time(x2<-ddply(data_50moreR,.(user),tst2))

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

func<-function(x)
{
  x=data.table(x)
  z<-x[1,]$location_id
  print(unique(x$user))
  for(i in 2:nrow(x))
  {
    if(x[i,]$location_id %in% z)
    {
      x[i,]$label<-0
      z=rbind(z,x[i,]$location_id)
    }
  }
  
  
  return (x)
}
# q=factor(x$label)
# qplot(q) 
# summary(q)

# data_50more$label=1


#-------------------getting returning time diffecene between 
#-------------------first visit to a location and its next one.
func7<-function(x)
{
  data_1_label_0<-x[x$label==0,]
  data_1_label_1<-x[x$label==1,]
  if(nrow(data_1_label_0)<1 | nrow(data_1_label_1)<1)
  {
    data_1_label_0_1st=data_1_label_0
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
                                                             data_1_label_0_1st[i,]$location_id,]$`check-in time`,
                                            data_1_label_0_1st[i,]$`check-in time`)
    # data_1_label_1[which(data_1_label_1$location_id == data_1_label_0_1st[i,]$location_id), ]$`check-in_time`
  }
  
  return (data_1_label_0_1st)
}

data_50more_wlabels_diff=ddply(x2,.(user),func7)

# data_50more_wlabels_diff$diff1
