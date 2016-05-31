
#traning the model with Nx2 matrix and creating a sequence matrix for testing 
#and check for every column 2 state with the predicted state generated using
#the model 
library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)

f="D:/StayPoints.txt"
table_user_training=read.table(sep = ",",f,header = TRUE,
                                          stringsAsFactors=FALSE)


escape=0
for(q in 
    4
    # unique(table_user_training$UserID)
)

{
  # q=3
  set.seed(1)
  hellooo=unique(table_user_training[,"sequenceNo"])
  hellnoo=sample(hellooo,floor(length(hellooo)*.7),replace = F)
  
  if( is.na(hellnoo[1]))
  { 
    escape=escape+1
    next;
    
  }
  table_user_=table_user_training
  table_user_70=table_user_[table_user_$sequenceNo %in% hellnoo,]
  table_user_i_training<-table_user_70[table_user_70$UserID==q,]
  table_user_testing=table_user_[!table_user_$sequenceNo %in% hellnoo,]
  table_user_i_testing=table_user_testing[table_user_testing$UserID==q,]
  
  
  table_user=table_user_i_training
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
  
  
  
  qq=aggregate(table_user1[, c("x","y")], list(dbnew$cluster), median)
  qr=mercator(qq[,2:3],inverse = TRUE)
  
  cols <- rainbow(length(unique(dbnew$cluster)),alpha = NULL)
  # pdf$colors <- cols[unclass(pdf$Study)]
  
  if(length(unique(dbnew$cluster))==1)
    next
  
  table_user1=as.data.frame(cbind(table_user1,location=dbnew$cluster))
  
  sequences=aggregate(table_user1[,"location"], 
                      list(year(table_user1$date),
                           day(table_user1$date),
                           month(table_user1$date),
                           table_user1$UserID), list)
  names(sequences)[5]=paste("location")
  
  
  mat=as.matrix(seq.matrix(sequences$location))
  
  vec=as.vector(unlist(sequences$location))
  #-----------------------------------training the markov chain 
  #using a single sequence
  # mcFit <- markovchainFit(data=vec,method = "laplace")
  
  #using 2d transition each row is a reletion between two nodes
  mcFit<-markovchainFit(data=mat,method= "mle",confidencelevel=.95)
  myMc<-as(mcFit$estimate, "markovchain")
  plot(myMc)
#--------------------------------testing---------------------------------------------
  table_user_testing<-table_user_i_testing
table_user_testing$date = as.POSIXct(paste(table_user_testing[,9],table_user_testing[,10]),tz = "GMT") 
# attributes(table_user_testing$date)$tzone <- "Asia/Shanghai"
table_user_testing$hour=hour(table_user_testing$date)
table_user_testing$day=weekdays(table_user_testing$date)
ll=cbind(table_user_testing$Long,table_user_testing$Lat)
xy = mercator(ll)#/10000000

table_user_testing$x=xy[,1]
table_user_testing$y=xy[,2]



# predict.dbscan(dbnew,data = table_user1[,c("x","y")],newdata =table_user2[1,c("x","y")] )

table_user2=table_user_testing#[table_user_testing$day=="Saturday",]
location=predict.dbscan(dbnew,data = table_user1[,c("x","y")],newdata =table_user2[,c("x","y")] )
# gridcell=predict.dbscan(dbscan_grid,data = table_user[,c("x","y")],newdata =table_user2[,c("x","y")] )

table_user2=as.data.frame(cbind(table_user2,location))
#----------------------------------------------------------

sequences_testing2=aggregate(table_user2[,c("location")], 
                            list(table_user2$sequenceNo), list)
names(sequences_testing2)[2]=paste("location")
#----------------------------------------------------------
k=0
z=0
s=0
t=0
for(i in sequences_testing2$Group.1)
{
  location=predict.dbscan(dbnew,data = table_user1[,c("x","y")],newdata =table_user2[table_user2$sequenceNo==i,c("x","y")] )
  points=cbind(table_user2[table_user2$sequenceNo==i,c("x","y","hour","day","UserID")],location)
  if(nrow(points)<2)
    next
  
    for(j in 1:(nrow(points)-1))
    {
      tryCatch(
        {
          predictedloc=predict(mcFit$estimate,location[j],n.ahead=1)  
        },
        error=function(cond) {
          message(cond)
          
        }
      )
      if(predictedloc==0)
      {
        list_x<-recommender(table_user_70,points[j,],points[j,]$UserID)
        #
        if(any(dist(points[j,c("x","y")],list_x)<1000))
         # s=s+1 
          z=z+1
        else
          # t=t+1
          k=k+1
        
      }
      else 
        if(predictedloc==location[j+1])
        z=z+1
      else
        k=k+1
      
      
    }
}
#---------------------------------------------------------------

cat(sprintf("user No.:\ %i\ accuracy=\ %f\ \n", q, (100*z)/(k+z)))
write.table(file = "d://helloo_new4.txt",x = sprintf("user No.:\ %i\ accuracy=\ %f\ ", q, (100*z)/(k+z)),row.names = F,col.names = F,append = T)
#   
print("Number of ture:")
print(z)
print("---------------------------------------------------------")
print("all:")
print(k+z)
}