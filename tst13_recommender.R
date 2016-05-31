library(recommenderlab)
library(reshape2)
library(ggplot2)
library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)

f="D:/All_StayPoints.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,
                                          stringsAsFactors=FALSE))



q=c(1:20)
set.seed(1)
hellooo=unique(table_user_training[table_user_training$UserID%in%q ,"sequenceNo"])
hellnoo=sample(hellooo,floor(length(hellooo)*.7),replace = F)

table_user_=table_user_training[table_user_training$UserID%in%q,]
table_user=table_user_[table_user_$sequenceNo %in% hellnoo,]
table_user_testing=table_user_[!table_user_$sequenceNo %in% hellnoo,]

table_user$date = as.POSIXct(paste(table_user[,9],table_user[,10]),tz = "GMT") 
# attributes(table_user$date)$tzone <- "Asia/Shanghai"

table_user$hour=hour(table_user$date)
table_user$day=weekdays(table_user$date)
ll=cbind(table_user$Long,table_user$Lat)
xy = mercator(ll)#/10000000

table_user$x=xy[,1]
table_user$y=xy[,2]


table_user1=table_user#[table_user$day=="Saturday",]

dbnew=dbscan(distm(ll),eps = 100,MinPts = 1,method = "dist")

# qq=aggregate(table_user1[, c("x","y")], list(dbnew$cluster), median)

table_user1=as.data.frame(cbind(table_user1,location=dbnew$cluster))

mat=matrix(0,0,nrow = length(unique(table_user1$UserID)),ncol = length(unique(table_user1$location)))
df=as.data.frame(mat,row.names =unique(table_user1$UserID))
names(df)=as.character(unique(table_user1$location))
# 
# mat[1,]=table(table_user1[table_user1$UserID==2,]$location)
# unique(table_user1$location)
# table(table_user1[table_user1$UserID==2,]$location)[[1]]
k=1
for(i in unique(table_user1$UserID))
{
  tab=as.data.frame(table(table_user1[table_user1$UserID==i,]$location))
  for(j  in sort(unique(table_user1[table_user1$UserID==i,]$location)))
    df[as.character(i),as.character(j)]=tab[tab$Var1==j,"Freq"]
  
  k=k+1
} 
df=df[ , ! apply( df , 2 , function(x) sum(x)>100 ) ]
dhl=as.matrix(df)

dhl[dhl==0]<-NA
# x=aggregate(table_user1[,c("location","UserID")],list(table_user1$location,table_user1$UserID),sum)
# df=df[-c(1,2)]
# str(x)

affinity.matrix<- as(dhl,"realRatingMatrix")
image(as(affinity.matrix,"matrix"))

image(affinity.matrix, main = "Raw ratings")


qplot(getRatings(affinity.matrix),
      main = "Histogram of normalized ratings", xlab = "Rating") 
hist(getRatings(affinity.matrix), breaks=100)
summary(getRatings(normalize(affinity.matrix, method = "Z-score"))) 
qplot(rowCounts(affinity.matrix), binwidth = 10, 
      main = "Movies Rated on average", 
      xlab = "# of users", 
      ylab = "# of movies rated")

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

aff_train<-affinity.matrix[!rownames(affinity.matrix)==4,]
aff_test<-affinity.matrix[rownames(affinity.matrix)==4,]
re <- Recommender(aff_train, method = "IBCF")
r <- predict(re,aff_test, n=5,type="ratings")

# bestN(r,n=3)
# topNList(re)
# names(getModel(re))
# 
as(r, "list")


# a=as(affinity.matrix[4],"matrix")
# # a[1:length(a)]<-NA
# # a[100:300]<-1
# a[a<4  ]<-NA
# b=as(a,"realRatingMatrix")
# z <- predict(re, b, n=5,type="ratings")
# # bestN(z,n=2)
# as(z, "list")