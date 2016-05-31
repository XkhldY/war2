#traning the model with Nx2 matrix and creating a sequence matrix for testing 
#and check for every column 2 state with the predicted state generated using
#the model 

# by running this code for all the users with similarity threshold of 40% 
# sharing the same places we found that variable (m) equal to this results 
# from entry [[2]] we found that almost all users share the same places 
# so we shall use recommender system directly with the whole dataset
# [[1]]
# [1]  0  3  4 30
# 
# [[2]]
# [1]  11  12  13  20  21  27  31  32  33  45  47  53  54  55  56  57  58  59
# [19]  61  63  64  70  71  72  75  76  77  78  79  80  81  86  88  90  91  93
# [37]  94  97  98  99 100 102 105 107 109 110 111 114 115 117 118 120 122 123
# [55] 125 127 132 133 136 141 143 146 147 148 149 150 152 154 157 158 160 161
# [73] 162 164 165 166 171 172
# 
# [[3]]
# [1] 16 28
# 
# [[4]]
# [1]  46 121 151
# 
# [[5]]
# [1]  49  60  74  87 116 159 170
# 
# [[6]]
# [1]  69 129
# 
# [[7]]
# [1] 108 138 139
# 
# [[8]]
# [1] 126 167
# 
# [[9]]
# [1] 153 163


library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("fpc");library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr);library(sets)

f="D:/All_StayPoints.txt"
table_user_training=data.frame(read.table(sep = ",",f,header = TRUE,
                                          stringsAsFactors=FALSE))


  table_user=table_user_training#[table_user_training$UserID<20,]
  
  table_user$date = as.POSIXct(paste(table_user[,9],table_user[,10]),tz = "GMT") 
  # attributes(table_user$date)$tzone <- "Asia/Shanghai"
  
  table_user$hour=hour(table_user$date)
  table_user$day=weekdays(table_user$date)
  
    ll=cbind(table_user$Long,table_user$Lat)
    xy = mercator(ll)#/10000000
    
    table_user$x=xy[,1]
    table_user$y=xy[,2]
    
    start.time <- Sys.time()
   
    dbnew=dbscan(distm(ll),eps = 100,MinPts = 4,method = "dist")
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
    table_user1=as.data.frame(cbind(table_user,location=dbnew$cluster))
    # sim1=matrix(data = NA,nrow = length(unique(table_user1$UserID)),ncol = length(unique(table_user1$UserID)))
    sim2=list(NULL)
    
    start.time <- Sys.time()
    
    for(i in 1:length(unique(table_user1$UserID)+1))
    {
      sim2[[i]]=i-1
      for(j in 1:length(unique(table_user1$UserID)))
        {
          A=gset(table_user1[table_user1$UserID==i-1,]$location)
          B=gset(table_user1[table_user1$UserID==j-1,]$location)
          if(gset_similarity(A, B, "Jaccard")>.40)
            sim2[[i]]=c(sim2[[i]],j-1)
          # sim1[i,j]=gset_similarity(A, B, "Jaccard")
          
          
      }
        
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
    
    # print(sim2)
    
    x=lapply(sim2,function(x) x[1]==x[2] && length(x)==2)
    m=sim2[x!=TRUE]
    
    z=NULL
    for(i in 1:length(m))
      for(j in 1:length(m))
        if(any(m[[i]]%in%m[[j]]) && i!=j)
        {
          z=as.data.frame(rbind(z,c(i,j)))
          m[[i]]=sort(unique(c(m[[i]],m[[j]])))
          # m[[j]]=c(m[[i]],m[[j]])
        }
    m=m[!duplicated(m)]
#     for(i in 1:length(m))
#     {
#       # train_test_markovchain1(m[[i]],m[[i]][1])
#       train_test_markovchain(m[[i]],m[[i]][1]) 
#     }
#     
#     recommender(m[[1]],0)
    