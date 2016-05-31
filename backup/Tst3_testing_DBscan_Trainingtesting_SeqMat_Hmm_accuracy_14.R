library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library(dbscan);library("lubridate")
library(markovchain);library(dplyr)

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_training_1-4.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_testing_1-4.txt"
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


dbscan_location=dbscan(table_user[,c("x","y")] ,eps =15,minPts = 1)
table_user=as.data.frame(cbind(table_user,location=dbscan_location$cluster))

#remove locations visited once
freq=table(table_user$location)
select=as.numeric(row.names(freq[freq>1]))

# table_user=table_user[table_user$location %in%select,]


dbscan_grid=dbscan(table_user[,c("x","y")] ,eps =1000,minPts = 1)
table_user=as.data.frame(cbind(table_user,gridcell=dbscan_grid$cluster))

# remove duplicate consectives 
# table_user=table_user[with(table_user, c(TRUE, diff(as.numeric(interaction(location, gridcell))) != 0)), ]

# remove duplicate consectives ,hint: not consider duplicate but are in different days
# table_user=table_user[table_user$location!=lag(table_user$location, default=1),]


sequences=aggregate(table_user[,c("location","gridcell")], 
                    list(year(table_user$date),
                         day(table_user$date),
                         month(table_user$date),
                         table_user$UserID), list)

# remove duplicate consectives but not removing corsponding state
# sequences$location=lapply(X = sequences$location,FUN = function(x) x[x!=c(x[-1], FALSE)])

# lapply(X = sequences$location,FUN = function(x) x=x[!duplicated(x,fromLast = TRUE)])

#checking which sequence number is the common
x=NULL
for(i in 1:nrow(sequences)) x[i]=(length(sequences$location[[i]]))
# mean(x)
seqLen<-row.names(sort(table(x),decreasing=TRUE))[1]
seqLen="6"

#ceating the sequence matrix (observations matrix)
seq_matrix_all=NULL
cell_matrix_all=NULL
for(i in 1:length(sequences$location))
  if(length(sequences$location[[i]])==as.numeric(seqLen))
  {
    seq_matrix_all=rbind(seq_matrix_all,as.character(sequences$location[[i]]))
    cell_matrix_all=rbind(cell_matrix_all,sequences$gridcell[[i]])
  }

#random IDs of 70% of the data as the training set and the remaining are the test set
set.seed(2)
training_ids=sample(1:nrow(seq_matrix_all),ceiling(nrow(seq_matrix_all)*.7),replace = FALSE)
#training sequences
seq_matrix=seq_matrix_all[training_ids,]
#testing sequences
seq_matrix_test=seq_matrix_all[-training_ids,]

#the states matrix for every location in the sequence matrix
cell_matrix=cell_matrix_all[training_ids,]
# cell_matrix_test=cell_matrix_all[-training_ids,]

reunit=matrix(c(Matrix::as.vector(t(seq_matrix)),Matrix::as.vector(t(cell_matrix))),ncol=2)


#-------------------------------------------------------- emission matrix
em=emission.matrix(reunit[,2],reunit[,1])

# emission=matrix(0,nrow = length(unique(reunit[,2]))
#                 ,ncol = length(unique(reunit[,1])))
# dt=data.frame(x=reunit[,2],y=reunit[,1])
# k=unique(reunit[,2])
# q=unique(reunit[,1])
# # loop to get the emission matrix
# for(i in 1:length(k))
#   for(j in 1:length(q))
#   {
#     emission[i,j]=nrow(dt[dt$x==q[j] & dt$y==k[i],])/nrow(dt[dt$y==k[i],])
#   }
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

training_transitionMatrix<-mcFit$estimate@transitionMatrix

start=NULL
start=matrix(1/states_no,nrow = 1,ncol = states_no)


hmm=initHMM(States =states_names,Symbols = Symbols_names
            # ,transProbs = training_transitionMatrix
            # ,transProbs = transition_mat
            ,emissionProbs = em
            # ,startProbs =start
            )

bw=baumWelch(hmm,seq_matrix,1)
#----------------------------------------------------------

k=0
y=0
m=NULL
tst=as.numeric(seqLen)-1
for(i in 1:nrow(seq_matrix_test))
  tryCatch(
    {
      tst_matrix=matrix(c(seq_matrix_test[i,1:tst]),ncol = tst,nrow=symbols_no,byrow = TRUE)
      tst_matrix=cbind(tst_matrix,Symbols_names)
      # h=apply(x,MARGIN = 1,function(y) forward(bw$hmm,y))
      for(j in 1:nrow(tst_matrix))
        m[j]=max(forward(bw$hmm,tst_matrix[j,])[,tst+1])
      
      id=which.max(m)
      #           print(id)   
      #           # print(forward(hmm,seq_matrix_test[i,1:3]))
      # print(seq_matrix_test[i,3])
      print(tst_matrix[id,tst+1])
      print(seq_matrix_test[i,tst+1]==tst_matrix[id,tst+1])
      if(seq_matrix_test[i,tst+1]==tst_matrix[id,tst+1])
        {
        
          k=k+1
        }
      else
          y=y+1
      
      #   message("This is the 'try' part")
    },
    error=function(cond) {
      
      message("new points found")
      
    }
  )    


print(y)
print(k)
print((100*k)/(y+k))
