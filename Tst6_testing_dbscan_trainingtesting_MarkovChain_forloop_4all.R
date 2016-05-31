library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr)


f="D:/work/Research/location research/datasets/Geolife/StayPoints_withSeq_10min.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,stringsAsFactors=FALSE))


escape=0
for(q in 
    unique(table_user_training$UserID)
    )
    {
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
    
      
    
    
    # table_user=as.data.frame(rbind(table_user_training,table_user_testing))
    
    table_user$date = as.POSIXct(paste(table_user[,9],table_user[,10]),tz = "GMT") 
    # attributes(table_user$date)$tzone <- "Asia/Shanghai"
    
    table_user$hour=hour(table_user$date)
    table_user$day=weekdays(table_user$date)
    ll=cbind(table_user$Long,table_user$Lat)
    xy = mercator(ll)#/10000000
    
    table_user$x=xy[,1]
    table_user$y=xy[,2]
    
    
    table_user1=table_user#[table_user$day=="Saturday",]
    
    dbscan_location=dbscan(table_user1[,c("x","y")] ,eps =200,MinPts = 1)
    plot(dbscan_location,table_user1[,c("x","y")])
    table(dbscan_location$cluster)
    
    
    
    table_user1=as.data.frame(cbind(table_user1,location=dbscan_location$cluster))
    
    #remove locations visited once
    freq=table(table_user1$location)
    select=as.numeric(row.names(freq[freq>1]))
    
    # table_user1=table_user1[table_user1$location %in%select,]
    
    # remove duplicate consectives 
    # table_user1=table_user1[with(table_user1, c(TRUE, diff(as.numeric(interaction(location, gridcell))) != 0)), ]
    
    # remove duplicate consectives ,hint: not consider duplicate but are in different days
    # table_user1=table_user1[table_user1$location!=lag(table_user1$location, default=1),]
    
    
    sequences=aggregate(table_user1[,"location"], 
                        list(year(table_user1$date),
                             day(table_user1$date),
                             month(table_user1$date),
                             table_user1$UserID), list)
    names(sequences)[5]=paste("location")
    
    # remove duplicate consectives but not removing corsponding state
    # sequences$location=lapply(X = sequences$location,FUN = function(x) x[x!=c(x[-1], FALSE)])
    
    # lapply(X = sequences$location,FUN = function(x) x=x[!duplicated(x,fromLast = TRUE)])
    
    #checking which sequence number is the common
    x=NULL
    for(i in 1:nrow(sequences)) x[i]=(length(sequences$location[[i]]))
    # mean(x)
    seqLen<-row.names(sort(table(x),decreasing=TRUE))[1]
    if (is.null(seqLen)) {
      escape=escape+1
      next;
      
    }
    # seqLen="5"
    
    #ceating the sequence matrix (observations matrix)
    seq_matrix_all=NULL
    cell_matrix_all=NULL
    for(i in 1:length(sequences$location))
      if(length(sequences$location[[i]])==as.numeric(seqLen))
      {
        seq_matrix_all=rbind(seq_matrix_all,as.character(sequences$location[[i]]))
        # cell_matrix_all=rbind(cell_matrix_all,sequences$gridcell[[i]])
      }
    
    #random IDs of 70% of the data as the training set and the remaining are the test set
    # set.seed(2)
    # training_ids=sample(1:nrow(seq_matrix_all),ceiling(nrow(seq_matrix_all)*.7),replace = FALSE)
    #training sequences
    seq_matrix=seq_matrix_all
    cell_matrix=cell_matrix_all
    #testing sequences
    # seq_matrix_test=seq_matrix_all[-training_ids,]
    
    #the states matrix for every location in the sequence matrix
    
    # cell_matrix_test=cell_matrix_all[-training_ids,]
    
    # reunit=matrix(c(Matrix::as.vector(t(seq_matrix)),Matrix::as.vector(t(cell_matrix))),ncol=2)
    
    
    #-------------------------------------------------------- emission matrix
    # em=emission.matrix(reunit[,2],reunit[,1])
    
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
    # 
    # symbols_no=length(unique(Matrix::as.vector(seq_matrix)))
    # Symbols_names=as.character(unique(Matrix::as.vector(seq_matrix)))
    # states_no=length(unique(Matrix::as.vector(cell_matrix)))
    # states_names=as.character(unique(Matrix::as.vector(cell_matrix)))
    
    # transition_mat=matrix(0,nrow = states_no,ncol = states_no)
    # emission_mat=matrix(1/symbols_no,nrow=states_no,ncol= symbols_no)
    
    # using MCTM package to get the transition matix (training)
    #   State_seq<- Matrix::as.vector(cell_matrix)
    #   transition_mat=TransMatrix(cell_matrix)
    #   
    
    mcFit <- markovchainFit(data=seq_matrix)
    
    
    
    # training_transitionMatrix<-mcFit$estimate@transitionMatrix
    # 
    # start=NULL
    # start=matrix(1/states_no,nrow = 1,ncol = states_no)
    
    # 
    # hmm=initHMM(States =states_names,Symbols = Symbols_names
    #             # ,transProbs = training_transitionMatrix
    #             # ,transProbs = transition_mat
    #             ,emissionProbs = em
    #             # ,startProbs =start
    # )
    # 
    # bw=baumWelch(hmm,seq_matrix,10)
    #----------------------------------------------------------
    ################################################
    ################################################
    #testing 
    #----------------------------------------------------------
    
    
    table_user_testing$date = as.POSIXct(paste(table_user_testing[,9],table_user_testing[,10]),tz = "GMT") 
    # attributes(table_user_testing$date)$tzone <- "Asia/Shanghai"
    table_user_testing$hour=hour(table_user_testing$date)
    table_user_testing$day=weekdays(table_user_testing$date)
    ll=cbind(table_user_testing$Long,table_user_testing$Lat)
    xy = mercator(ll)#/10000000
    
    table_user_testing$x=xy[,1]
    table_user_testing$y=xy[,2]
    
    
    table_user2=table_user_testing#[table_user_testing$day=="Saturday",]
    location=predict.dbscan(dbscan_location,data = table_user1[,c("x","y")],newdata =table_user2[,c("x","y")] )
    # gridcell=predict.dbscan(dbscan_grid,data = table_user[,c("x","y")],newdata =table_user2[,c("x","y")] )
    
    table_user2=as.data.frame(cbind(table_user2,location))
    # table_user2=as.data.frame(cbind(table_user2,gridcell))
    
    plot(table_user2[,c("x","y")],col=table_user2$location)
    # plot(table_user2[,c("x","y")],col=table_user2$gridcell)
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
      
      #   if()
      #    {
      #     t=t+1 
      #   }
      #   else
      if(len>2 && !any(sequences_testing$location[[i]][1:len]==0))
      {
        print("fine")
        print(i)
        
        tryCatch(
          {
            a=predict(mcFit$estimate,sequences_testing$location[[i]][1:len-1],n.ahead=1)           
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
    write.table(file = "d://helloo_10mins.txt",x = sprintf("user No.:\ %i\ accuracy=\ %f\ ", q, (100*j)/(j+k)),row.names = F,col.names = F,append = T)
    
    
}