#training/testing markov chain

library(HMM);library("geosphere");library("ggmap");library("ggplot2")
library("fossil");library(LPCM);library("lubridate")
library(markovchain);library(dplyr);library(leaflet)
library(magrittr)
library(data.table)
# library("fpc")
library(dbscan)

# training<-fread("D:\\training.csv")
# testing<-fread("D:\\testing.csv")
result=data.frame("userID"=0,"N_U_train"=0,"N_U_test"=0,"dbscan"=0,"ACC"=0,"match"=0,"mismatch"=0,"noise_novel"=0)
q=1
for(ij in unique(training[training$user<10,]$user))
{
  training_portion=training[training$user==ij,]
  
  
  testing_portion=testing[testing$user==ij,]
  
  result[q,]$N_U_train=length(unique(training_portion$user))
  result[q,]$N_U_test=length(unique(testing_portion$user))
  # escape=0
  
  # data_50more<-ddply(training_portion, .(user),)
  ll=cbind(training_portion$longitude,training_portion$latitude)
  xy = mercator(ll)#/10000000
  
  dbnew=dbscan(dist(xy),eps = 100,minPts = 1,method = "dist")
  
  result[q,]$dbscan=paste(dbnew$eps,":",dbnew$minPts)
  
  training_portion$dbscan=dbnew$cluster
  
#   sequences=aggregate(training_portion$dbscan, 
#                       list(year(training_portion$`check-in time`),
#                            day(training_portion$`check-in time`),
#                            month(training_portion$`check-in time`),
#                            training_portion$user), list)
#   names(sequences)[5]=paste("location")
  
  
  # mat=as.matrix(seq.matrix(sequences$location))
  
  # mcFit<-markovchainFit(data=mat,method= "mle")
  
  mcFit<-markovchainFit(data=training_portion$dbscan,method= "mle")
  
  # verifyMarkovProperty(as.character(dbnew$cluster))
  # mcFit<-markovchain::fitHigherOrder(mat,order = 2)
  
  xx=cbind(testing_portion$longitude,testing_portion$latitude)
  xxy=mercator(xx)
  location=predict(dbnew,data = xy,newdata =xxy)
  
  testing_portion$dbscan=location
  #---------------------------------------------------------
  j=0
  k=0
  a=0
  for(i in 1:nrow(testing_portion)-1)
  {
    tryCatch(
      {
        a[i]=predict(mcFit$estimate,testing_portion[i,]$dbscan,n.ahead=1)
        # print(a)
        if(a[i]==testing_portion[i+1,]$dbscan)
        {
          j=j+1
        }
        else
          k=k+1
      },
      error=function(cond) {
        #  message(cond)
        
      }
    )    
  }
  result[q,]$ACC=(100*j)/(j+k)
  result[q,]$userID=testing_portion[1,]$user
  result[q,]$match=j
  result[q,]$mismatch=k
  result[q,]$noise_novel=length(location)-(k+j)
  q=q+1
} 

write.table(result,file = "d://markovChain.csv",row.names = F,quote = F,sep = ",")


#       cat(sprintf("user No.:\ %i\ accuracy=\ %f\ \n", training_portion[i,]$user, (100*j)/(j+k)))
#       write.table(file = "d://helloo_all_new.txt",x = sprintf("user No.:\ %i\ accuracy=\ %f\ Number of matches: \ %i\ Number of mismatches: \ %i  \n", training_portion[i,]$user,(100*j)/(j+k),j,k),row.names = F,col.names = F,quote = F,append = T)
#       
#       cat(sprintf("Number of matches:\ %i\ Number of mismatches:\ %i\ totalno.\ %i\ \n",j,k,length(location)))

#       cat("N_U_train","N_U_test","dbscan","ACC","match","mismatch","noise")  
# }    
#       sequences=aggregate(testing_portion$dbscan, 
#                           list(year(testing_portion$`check-in time`),
#                                day(testing_portion$`check-in time`),
#                                month(testing_portion$`check-in time`),
#                                testing_portion$user), list)
#       names(sequences)[5]=paste("location")
#       
#       
#       test.seq=as.matrix(seq.matrix(sequences$location))
#       
#       
#       #remove moving  from node to the same node
#       # test.seq=test.seq[!apply(  test.seq,1,function(x) x[1]==x[2]),]
#       for(i in 1:nrow(test.seq))
#       {
#         tryCatch(
#           {
#             # a=markovchainSequence(n=1,markovchain=myMc, t0=test.seq[i,1],include.t0 =TRUE )
#             a=predict(mcFit$estimate,test.seq[i,1],n.ahead=1)           
#             # print(a)
#             if(a==test.seq[i,2])
#             {
#               j=j+1
#             }
#             else
#               k=k+1
#           },
#           error=function(cond) {
#             message(cond)
#             
#           }
#         )    
#       }


