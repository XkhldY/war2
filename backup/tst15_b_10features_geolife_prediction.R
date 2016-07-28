#novelity check for both geolife and gowalla
library(rpart)
library(pROC)
library(data.table)
library(caret)
require(bit64) 
# training<-fread("D:\\training_geolife.csv")
# testing<-fread("D:\\testing_geolife.csv")
training<-fread("D:\\training.csv",nrows = 100000)
testing<-fread("D:\\testing.csv",nrows=100000)

# deepnet<-fread("D:\\deepnet.csv")
# logit<-fread("D:\\logit.csv")
result=data.frame("UserID"=0,"allAcc"=0,"historicalAcc"=0,"spatialAcc"=0,"temporalAcc"=0)
# result[i]$UserID=i
# result[i]$allAcc=allConf$overall[1]
# result[i]$historicalAcc=historicalConf$overall[1]
# result[i]$spatialAcc=spatialConf$overall[1]
# result[i]$temporalAcc=temporalConf$overall[1]
q=1
for(i in unique(training$user))
{

  t<-training[training$user==i,]
  test<-testing[testing$user==i,]
  t$label<-as.character(t$label)
  # t$HOW<-log10(t$HOW)
  temporal<-rpart(label~HOW+timediff+DOW+HOD,method = "class",data = t)
  Historical<-rpart(label~Nratio+NoOfDays+preNovel+distinct,method = "class",data = t)
  spatial<-rpart(label~distdiff+VisitingRatio,method = "class",data = t)
  
  all<-rpart(label~HOW+timediff+DOW+HOD
             +Nratio+NoOfDays+preNovel+distinct
             +distdiff+VisitingRatio,method = "class",data = t)
  
  
  # printcp(temporal) # display the results 
  # printcp(Historical)
  # printcp(spatial)
  # printcp(all)
  
  # plot(all, uniform=TRUE,
  #      main="Classification Tree ")
  # text(all, use.n=TRUE, all=TRUE, cex=.8)
  # 
  # plot(temporal, uniform=TRUE,
  #      main="Classification Tree ")
  # text(temporal, use.n=TRUE, all=TRUE, cex=.8)
  # 
  # plot(spatial, uniform=TRUE,
  #      main="Classification Tree ")
  # text(spatial, use.n=TRUE, all=TRUE, cex=.8)
  # 
  # plot(Historical, uniform=TRUE,
  #      main="Classification Tree ")
  # text(Historical, use.n=TRUE, all=TRUE, cex=.8)
  
  tryCatch(
    {
      temporalpredict<-predict(temporal,type = "class",newdata = test)
      historicalpredict<-predict(Historical,type = "class",newdata = test)
      spatialpredict<-predict(spatial,type = "class",newdata = test)
      allpredict<-predict(all,type = "class",newdata = test)
  
      temporalConf=confusionMatrix(temporalpredict,test$label)
      historicalConf=confusionMatrix(historicalpredict,test$label)
      spatialConf=confusionMatrix(spatialpredict,test$label)
      
      allConf=confusionMatrix(allpredict,test$label)
      
      result[q,]$UserID=i
      result[q,]$allAcc=allConf$overall[1]
      result[q,]$historicalAcc=historicalConf$overall[1]
      result[q,]$spatialAcc=spatialConf$overall[1]
      result[q,]$temporalAcc=temporalConf$overall[1]
      q=q+1
    },
    error=function(cond) {
      message(cond,"userID= ",i)
      
      
      
    }
  )    
 
  

  
  # deepConf=confusionMatrix(deepnet$predict,testing$label)
  # logitconf=confusionMatrix(logit$predict,testing$label)
  

}

write.table(result,file = "d://accuracy_gowalla_tree.csv",row.names = F,quote = F,sep = ",")


