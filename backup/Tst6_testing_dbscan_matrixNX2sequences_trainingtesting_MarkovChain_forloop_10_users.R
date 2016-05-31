library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)

f="D:/StayPoints.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))


escape=0
for(q in 
    # 1
    unique(table_user_training$UserID)
)
{
  # q=0
  set.seed(1)
  hellooo=unique(table_user_training[table_user_training$UserID==q ,"sequenceNo"])
  hellnoo=sample(hellooo,floor(length(hellooo)*.7),replace = F)
  
  if( is.na(hellnoo[1]))
  { 
    escape=escape+1
    next;
    
  }
  table_user_=table_user_training[table_user_training$UserID==q,]
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

  dbnew=dbscan(distm(ll),eps = 100,MinPts = 2,method = "dist")



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
#checking which sequence number is the common
# x=NULL
# for(i in 1:nrow(sequences)) x[i]=(length(sequences$location[[i]]))
# # mean(x)
# seqLen<-row.names(sort(table(x),decreasing=TRUE))[1]
# 
# if (is.null(seqLen)) {
#   escape=escape+1
#   next;
#   
# }
# 
# seq_matrix_all=NULL
# 
# for(i in 1:length(sequences$location))
#   if(length(sequences$location[[i]])==as.numeric(seqLen))
#   {
#     seq_matrix_all=rbind(seq_matrix_all,as.character(sequences$location[[i]]))
#     # cell_matrix_all=rbind(cell_matrix_all,sequences$gridcell[[i]])
#   }
# 
# seq_matrix=seq_matrix_all

#-----------------------------------training the markov chain 
#using a single sequence
# mcFit <- markovchainFit(data=vec,method = "laplace")

#using 2d transition each row is a reletion between two nodes
mcFit<-markovchainFit(data=mat,method= "mle",confidencelevel=.95)
myMc<-as(mcFit$estimate, "markovchain")
plot(myMc)
#---------------------------------------------------------
table_user_testing$date = as.POSIXct(paste(table_user_testing[,9],table_user_testing[,10]),tz = "GMT") 
# attributes(table_user_testing$date)$tzone <- "Asia/Shanghai"
table_user_testing$hour=hour(table_user_testing$date)
table_user_testing$day=weekdays(table_user_testing$date)
ll=cbind(table_user_testing$Long,table_user_testing$Lat)
xy = mercator(ll)#/10000000

table_user_testing$x=xy[,1]
table_user_testing$y=xy[,2]


table_user2=table_user_testing#[table_user_testing$day=="Saturday",]
location=predict.dbscan(dbnew,data = table_user1[,c("x","y")],newdata =table_user2[,c("x","y")] )
# gridcell=predict.dbscan(dbscan_grid,data = table_user[,c("x","y")],newdata =table_user2[,c("x","y")] )

table_user2=as.data.frame(cbind(table_user2,location))
#----------------------------------------------------------

sequences_testing=aggregate(table_user2[,c("location")], 
                            list(year(table_user2$date),
                                 day(table_user2$date),
                                 month(table_user2$date),
                                 table_user2$UserID), list)

names(sequences_testing)[5]=paste("location")
a=0
j=0
k=0
t=0
# (nrow(sequences_testing)-1)
for(i in 1:nrow(sequences_testing))
{
  
  len=length(sequences_testing$location[[i]])
  
  if(len>2 && !any(sequences_testing$location[[i]][1:len]==0))
  {
    print("fine")
    print(i)
    
    tryCatch(
      {
        a=predict(mcFit$estimate,sequences_testing$location[[i]][len-1],n.ahead=1)           
        print(a)
        
        if(a==sequences_testing$location[[i]][len])
        {
          j=j+1
        }
        else
          k=k+1
      },
      error=function(cond) {
        message(cond)
        
      }
    )    
    
  }
}

cat(sprintf("user No.:\ %i\ accuracy=\ %f\ \n", q, (100*j)/(j+k)))
write.table(file = "d://helloo_hhhh.txt",x = sprintf("user No.:\ %i\ accuracy=\ %f\ ", q, (100*j)/(j+k)),row.names = F,col.names = F,append = T)


}
#---------------------------------------------------------
print("number of StayPoints")
print(nrow(table_user1))
print("number of POIs")
print(length(unique(dbnew$cluster))-1)
