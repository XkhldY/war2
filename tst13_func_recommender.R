library(recommenderlab)
library(reshape2)
library(ggplot2)
library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)
library(data.table)


recommender<-function(table_user_70,points,userID)
{
  # table_user_70
  #list_x=recommned(table_user_70,
  day1<-points[1,]$day
  hour1<-points[1,]$hour
  # ,points$UserID)
  # table_user_recommed<-table_user_70[(table_user_70$hour >=hour) &&(table_user_70$day== day),]
  
  
  table_user_70$date = as.POSIXct(paste(table_user_70[,9],table_user_70[,10]),tz = "GMT") 
  # attributes(table_user_70$date)$tzone <- "Asia/Shanghai"
  
  table_user_70$hour=hour(table_user_70$date)
  table_user_70$day=weekdays(table_user_70$date)
  
  table_user_recommed<-table_user_70[table_user_70$hour>=hour1 & table_user_70$day==day1,]
  
  
  ll=cbind(table_user_recommed$Long,table_user_recommed$Lat)
  xy = mercator(ll)#/10000000
  
  table_user_recommed$x=xy[,1]
  table_user_recommed$y=xy[,2]
  
  
  # table_user_70=table_user_70#[table_user$day=="Saturday",]
  
  dbnew=dbscan(distm(ll),eps = 100,MinPts = 1,method = "dist")
  
  # qq=aggregate(table_user_70[, c("x","y")], list(dbnew$cluster), median)
  
  table_user_recommed=as.data.frame(cbind(table_user_recommed,location=dbnew$cluster))
  
  mat=matrix(0,0,nrow = length(unique(table_user_recommed$UserID)),ncol = length(unique(table_user_recommed$location)))
  df=as.data.frame(mat,row.names =unique(table_user_recommed$UserID))
  names(df)=as.character(unique(table_user_recommed$location))
  # 
  # mat[1,]=table(table_user_recommed[table_user_recommed$UserID==2,]$location)
  # unique(table_user_recommed$location)
  # table(table_user_recommed[table_user_recommed$UserID==2,]$location)[[1]]
  k=1
  for(i in unique(table_user_recommed$UserID))
  {
    tab=as.data.frame(table(table_user_recommed[table_user_recommed$UserID==i,]$location))
    for(j  in sort(unique(table_user_recommed[table_user_recommed$UserID==i,]$location)))
      df[as.character(i),as.character(j)]=tab[tab$Var1==j,"Freq"]
    
    k=k+1
  } 
  df=df[ , ! apply( df , 2 , function(x) sum(x)>100 ) ]
  dhl=as.matrix(df)
  
  dhl[dhl==0]<-NA
  
  
  # x=aggregate(table_user_recommed[,c("location","UserID")],list(table_user_recommed$location,table_user_recommed$UserID),sum)
  # df=df[-c(1,2)]
  # str(x)
  
  affinity.matrix<- as(dhl,"realRatingMatrix")
  
  # image(as(affinity.matrix,"matrix"))
  # 
  # image(affinity.matrix, main = "Raw ratings")
  # 
  # 
  # qplot(getRatings(affinity.matrix),
  #       main = "Histogram of normalized ratings", xlab = "Rating") 
  # hist(getRatings(affinity.matrix), breaks=100)
  # summary(getRatings(normalize(affinity.matrix, method = "Z-score"))) 
  # qplot(rowCounts(affinity.matrix), binwidth = 10, 
  #       main = "Movies Rated on average", 
  #       xlab = "# of users", 
  #       ylab = "# of movies rated")
  # 
  # recommenderRegistry$get_entries(dataType = "realRatingMatrix")
  
  
  aff_train<-affinity.matrix[!rownames(affinity.matrix)==userID,]
  
  aff_test<-affinity.matrix[rownames(affinity.matrix)==userID,]
  
  aff_test_mat=as(aff_test,"matrix")
  
  
  aff_test_mat[aff_test_mat<3  ]<-NA
  aff_test_2=as(aff_test_mat,"realRatingMatrix")
  
  re <- Recommender(aff_train, method = "IBCF")
  ratings1 <- predict(re, aff_test, n=5,type="topNList")
  ratings2 <- predict(re, aff_test_2, n=5,type="topNList")
#   as(bestN(ratings1,n=3),"list")
#   as(bestN(ratings2,n=3),"list")
  rate<-as(bestN(ratings1,n=3),"list")
  recommendations<-table_user_recommed[table_user_recommed$location %in%rate[[1]] ,c("x","y")]
  
  return(recommendations)
}