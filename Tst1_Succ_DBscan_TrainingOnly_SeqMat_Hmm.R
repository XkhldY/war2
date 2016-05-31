library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library(dbscan);library("lubridate")
library(markovchain)

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_training_4.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))

f="D:/work/Research/location research/datasets/Geolife/BeijingStayPoints_testing_4.txt"
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

dbscan_grid=dbscan(table_user[,c("x","y")] ,eps =1700,minPts = 1)
table_user=as.data.frame(cbind(table_user,gridcell=dbscan_grid$cluster))


sequences=aggregate(table_user[,c("location","gridcell")], 
                    list(year(table_user$date),
                         day(table_user$date),
                         month(table_user$date)), list)

 # for(i in 1:length(sequences[[4]])) x[i]=(length(sequences$x[[i]]))

  seq_matrix=NULL
  cell_matrix=NULL
  for(i in 1:length(sequences[[4]]))
    if(length(sequences[[4]][[i]])==4)
      {
        seq_matrix=rbind(seq_matrix,as.character(sequences$location[[i]]))
        cell_matrix=rbind(cell_matrix,sequences$gridcell[[i]])
      }

  reunit=matrix(c(Matrix::as.vector(t(seq_matrix)),Matrix::as.vector(t(cell_matrix))),ncol=2)
  
  
  # em=emission.matrix(reunit[,2],reunit[,1])
  
  symbols_no=length(unique(Matrix::as.vector(seq_matrix)))
  Symbols_names=as.character(unique(Matrix::as.vector(seq_matrix)))
  states_no=length(unique(Matrix::as.vector(cell_matrix)))
  states_names=as.character(unique(Matrix::as.vector(cell_matrix)))
  
  transition_mat=matrix(0,nrow = states_no,ncol = states_no)
  emission_mat=matrix(1/symbols_no,nrow=states_no,ncol= symbols_no)
  
  # using MCTM package to get the transition matix (training)
#   State_seq<- Matrix::as.vector(cell_matrix)
#   transition_mat=TransMatrix(State_seq)
#   
  mcFit <- markovchainFit(data=cell_matrix)
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
              ,transProbs = training_transitionMatrix
              ,emissionProbs = emission_mat
              ,startProbs =start)
  
  
   bw=baumWelch(hmm,seq_matrix,10)
  

  
  
# transit=matrix(data = c(.1,.2,0,.2,.3,.4,.7,.5,.6),ncol = 3)

# x=emission.matrix(states = c("rains","cloudy","shiny"),observations = c("q","w","e","q","w","e","q","w","e","e","q"))

# observations = c("q","w","e","q","w"
#                  ,"e","q","w","e","e"
#                  ,"e","q","w","e","e"
#                  ,"e","q","w","e","e"
#                  ,"e","q","w","e","e"
#                  ,"e","q","w","e","e"
#                  ,"e","q","w","e","e");

# emission=matrix(data = c(.1,.2,.3,.8,.7,.5,.1,.1,.2),3)
# 
# states =c("rains","cloudy","shiny")
# symbols = c("q","w","e")
# emis=matrix(0,nrow=length(states),ncol = length(symbols))
# # transit=matrix(0,nrow=length(states),ncol = length(states))
# emis[,]=1/3



# startprob=c(.1,.4,.5)
# hmm=initHMM(States =states,Symbols = symbols
#             ,transProbs = transit,emissionProbs = emis,startProbs =startprob)
# 
# x=matrix(observations,nrow=7)
# # bw=baumWelch(hmm,seq[[1]]$observation,10)
# x[1,]<-"q"
# x[2,]<-"w"
# x[3,]<-"e"
# 
# bw=baumWelch(hmm,x,10)

# seq=list()
# for(i in 1:10)
#   seq[i]=list(simHMM(hmm = bw$hmm,length = 10))
# x=seq[1:2]
