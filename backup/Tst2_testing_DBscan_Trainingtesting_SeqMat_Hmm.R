library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library(dbscan);library("lubridate")
library(markovchain);library(dplyr)

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_training_4.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_testing_4.txt"
table_user_testing=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))
2
table_user=as.data.frame(rbind(table_user_training,table_user_testing))

table_user$date = as.POSIXct(paste(table_user[,8],table_user[,9]),tz = "GMT") 
# attributes(table_user$date)$tzone <- "Asia/Shanghai"
table_user$hour=hour(table_user$date)
table_user$day=weekdays(table_user$date)
ll=cbind(table_user$Long,table_user$Lat)
xy = mercator(ll)#/10000000

table_user$x=xy[,1]
table_user$y=xy[,2]


dbscan_location=dbscan(table_user[,c("x","y")] ,eps =100,minPts = 1)
table_user=as.data.frame(cbind(table_user,location=dbscan_location$cluster))

#remove locations visited once
freq=table(table_user$location)
select=as.numeric(row.names(freq[freq>1]))

freq1=table_user[table_user$location %in%select,]


dbscan_grid=dbscan(table_user[,c("x","y")] ,eps =1700,minPts = 1)
table_user=as.data.frame(cbind(table_user,gridcell=dbscan_grid$cluster))

# remove duplicate consectives 
# table_user=table_user[with(table_user, c(TRUE, diff(as.numeric(interaction(location, gridcell))) != 0)), ]

# remove duplicate consectives hint not consider duplicate but are in different days
# table_user1=table_user[table_user$location!=lag(table_user$location, default=1),]


sequences=aggregate(table_user[,c("location","gridcell")], 
                    list(year(table_user$date),
                         day(table_user$date),
                         month(table_user$date)), list)

# remove duplicate consectives but not removing corsponding state
# sequences$location=lapply(X = sequences$location,FUN = function(x) x[x!=c(x[-1], FALSE)])

# lapply(X = sequences$location,FUN = function(x) x=x[!duplicated(x,fromLast = TRUE)])

#checking which sequence number is the common
x=NULL
for(i in 1:length(sequences[[4]])) x[i]=(length(sequences$location[[i]]))
mean(x)

#ceating the sequence matrix (observations matrix)
seq_matrix_all=NULL
cell_matrix_all=NULL
for(i in 1:length(sequences$location))
  if(length(sequences$location[[i]])==ceiling(mean(x)))
  {
    seq_matrix_all=rbind(seq_matrix_all,as.character(sequences$location[[i]]))
    cell_matrix_all=rbind(cell_matrix_all,sequences$gridcell[[i]])
  }

#random IDs of 70% of the data as the training set and the remaining are the test set
set.seed(1)
training_ids=sample(1:nrow(seq_matrix_all),ceiling(nrow(seq_matrix_all)*.7),replace = FALSE)
#training sequences
seq_matrix=seq_matrix_all[training_ids,]
#testing sequences
seq_matrix_test=seq_matrix_all[-training_ids,]

#the states matrix for every location in the sequence matrix
cell_matrix=cell_matrix_all[training_ids,]
# cell_matrix_test=cell_matrix_all[-training_ids,]

reunit=matrix(c(Matrix::as.vector(t(seq_matrix)),Matrix::as.vector(t(cell_matrix))),ncol=2)


  em=emission.matrix(reunit[,2],reunit[,1])
 
 emission=matrix(0,nrow = length(unique(reunit[,2]))
                 ,ncol = length(unique(reunit[,1])))
 dt=data.frame(x=reunit[,2],y=reunit[,1])
 k=unique(reunit[,2])
 q=unique(reunit[,1])
 # loop to get the emission matrix
 for(i in 1:length(k))
   for(j in 1:length(q))
   {
     emission[i,j]=nrow(dt[dt$x==q[j] & dt$y==k[i],])/nrow(dt[dt$y==k[i],])
   }
 #--------------------------------------------------------

symbols_no=length(unique(Matrix::as.vector(seq_matrix)))
Symbols_names=as.character(unique(Matrix::as.vector(seq_matrix)))
states_no=length(unique(Matrix::as.vector(cell_matrix)))
states_names=as.character(unique(Matrix::as.vector(cell_matrix)))

# transition_mat=matrix(0,nrow = states_no,ncol = states_no)
emission_mat=matrix(1/symbols_no,nrow=states_no,ncol= symbols_no)

# using MCTM package to get the transition matix (training)
#   State_seq<- Matrix::as.vector(cell_matrix)
#   transition_mat=TransMatrix(cell_matrix)
#   

mcFit <- markovchainFit(data=cell_matrix)

# z=0
# cellen=length(unique(as.vector(cell_matrix)))
# trans=matrix(0,cellen,cellen)
# 
# 
# for(i in 1:cellen)
# {  
#   pres=unique(as.vector(cell_matrix))[i]
#     for(k in 1:cellen)
#     {
#       next1=unique(as.vector(cell_matrix))[k]
#       for(j in 1:nrow(cell_matrix))
#       {
#         for(t in 1:(ncol(cell_matrix)-1))
#         if (cell_matrix[j,t]==pres & cell_matrix[j,t+1]==next1)
#           z=z+1
#       }
#       trans[i,j]=z/sum(as.vector(cell_matrix)==pres)
#       z=0
#     }
#   }

training_transitionMatrix<-mcFit$estimate@transitionMatrix

start=NULL
start=matrix(1/states_no,nrow = 1,ncol = states_no)
#   j=1
#     for(i in 1:states_no)
#     {
#       x=unique(cell_matrix[,1])
#       if(cell_matrix[,1]==x[j])
#       {
#         start[i]=length(cell_matrix[cell_matrix[,1]==TRUE,2])/length(cell_matrix[,1])
#       }
#       j=j+1
#     }



hmm=initHMM(States =states_names,Symbols = Symbols_names
              # ,transProbs = training_transitionMatrix
            # ,transProbs = transition_mat
            ,emissionProbs = em
            ,startProbs =start)

   bw=baumWelch(hmm,seq_matrix,10)
  k=0
  m=NULL
  for(i in 1:nrow(seq_matrix_test))
      tryCatch(
        {
          x=matrix(c(seq_matrix_test[i,1:3]),ncol = 3,nrow=symbols_no,byrow = TRUE)
          x=cbind(x,Symbols_names)
          # h=apply(x,MARGIN = 1,function(y) forward(bw$hmm,y))
          for(j in 1:nrow(x))
          m[j]=max(forward(bw$hmm,x[j,])[,4])
            
          id=which.max(m)
#           print(id)   
#           # print(forward(hmm,seq_matrix_test[i,1:3]))
          # print(seq_matrix_test[i,3])
          print(x[id,4])
          print(seq_matrix_test[i,4]==x[id,4])
          k=k+1          
       #   message("This is the 'try' part")
        },
        error=function(cond) {
      
          message("Here's the original error message:")
      
        }
      )    

# forward(hmm,seq_matrix_test[6,1:3])

