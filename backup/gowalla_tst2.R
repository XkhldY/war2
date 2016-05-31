require("ggmap")
require(data.table)
require(plyr)
require(lubridate)
library(leaflet)
require(foreach)

data_50more_wlabels <- fread("D:\\work\\Research\\location research\\datasets\\loc-gowalla_Checkins_50more_wlabels.txt",header = TRUE)


data_50more_wlabels$`check-in_time`<-parse_date_time(paste(data_50more_wlabels$date,data_50more_wlabels$`check-in_time`),"Ymd HMS")

newdata <- data_50more_wlabels[order(`check-in_time`),] 
data_50more_wlabels <- subset(data_50more_wlabels, select = -c(date) )

# data_50more_wlabels$`check-in_time`<-parse_date_time2(data_50more_wlabels$`check-in_time`,"YmdHMS")

str(data_50more_wlabels)

#historgram for number of novel and return POIs
qplot(factor(head(data_50more_wlabels$label,10000)))


data_50more_wlabels_diff=ddply(x,.(user),func7)

#check the returing time mod the week days
qplot(factor(floor(data_50more_wlabels_diff$diff1)%%7))

# ggplot(data_50more_wlabels_diff, aes(x=diff1%%8)) + geom_density()


# data_50more_wlabels_diff[data_50more_wlabels_diff$diff1,]$diff1
#the 
data_50more_diff<-subset(data_50more_wlabels_diff,diff1<8)

qplot(factor(floor(data_50more_diff$diff1)))
ggplot(data_50more_diff, aes(x=diff1)) + geom_density()

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
#-----------------------------------------------------------

data_50more_wlabels_Nratio=ddply(data_50more_wlabels[data_50more_wlabels$user==2,],.(user),func10)


qplot(x =1:nrow(data_50more_wlabels_Nratio),geom=c("point", "smooth"),y =Nratio,data = data_50more_wlabels_Nratio)

func10<-function(x)
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
# func11<-function(x,data)
# {
#   x[1,"Nratio"]=nrow(data[data$label==1 & data$row_id<=x["row_id"],])/nrow(data[data$row_id<=x["row_id"],])
#   return(x)
# }
#-----------------------------------------------------------
# hell[1]$Nratio=nrow(hell[hell$label==1 & hell$row_id<=hell[1,"row_id"]])/nrow(hell[hell$row_id<=hell[1,"row_id"],])
#------------------------------------------------
# 
# 
# func8<-function(x)
# {
#   data_1_label_0<-x[x$label==0,]
#   data_1_label_1<-x[x$label==1,]
#   if(nrow(data_1_label_0)<1 | nrow(data_1_label_1)<1)
#   {
#     data_1_label_0_1st$diff1=0
#     print("error")
#     print(unique(x$user))
#     
#     return(data_1_label_0_1st)
#     
#   }
#   
#   
#   data_1_label_0_1st<-ddply(data_1_label_0, "location_id", head, 1)
#   print(unique(x$user))
#   data_1_label_0_1st$diff1=0
#   data_1_label_0_1st$diff1<-apply(data_1_label_0_1st,1,func9(x,data_1_label_1))
#   
#   
#   return (data_1_label_0_1st)
# }
# 
# 
# 
# func9<-function(x,data_1_label_1)
# {
#   x$diff1<- difftime(data_1_label_1[data_1_label_1$location_id ==
#                                       x$location_id,]$`check-in_time`,
#                      x$`check-in_time`)
#   return(x)
# }
# 
# 
# data_50more_wlabels_diff_func8=ddply(data_50more_wlabels,.(user),func8)

#-------------------------------------------------------------------


