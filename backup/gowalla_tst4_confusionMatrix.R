library(rpart)
library(data.table)
library(pROC)
library(caret)
# training<-fread("D:\\training.csv")
# testing<-fread("D:\\testing.csv")
training<-fread("D:\\training_100.csv")
testing<-fread("D:\\testing_100.csv")
t<-training#[1:10000,]
t$NextNovel<-as.character(t$NextNovel)
# t$HOW<-log10(t$HOW)
temporal<-rpart(NextNovel~HOW+timediff+DOW+HOD,method = "class",data = t)
Historical<-rpart(NextNovel~Nratio+NoOfDays+preNovel+distinct,method = "class",data = t)
spatial<-rpart(NextNovel~distdiff+VisitingRatio,method = "class",data = t)

all<-rpart(NextNovel~HOW+timediff+DOW+HOD
           +Nratio+NoOfDays+preNovel+distinct
           +distdiff+VisitingRatio,method = "class",data = t)


printcp(temporal) # display the results 
printcp(Historical)
printcp(spatial)
printcp(all)

plot(all, uniform=TRUE,
     main="Classification Tree ")
text(all, use.n=TRUE, all=TRUE, cex=.8)

plot(temporal, uniform=TRUE,
     main="Classification Tree ")
text(temporal, use.n=TRUE, all=TRUE, cex=.8)

plot(spatial, uniform=TRUE,
     main="Classification Tree ")
text(spatial, use.n=TRUE, all=TRUE, cex=.8)

plot(Historical, uniform=TRUE,
     main="Classification Tree ")
text(Historical, use.n=TRUE, all=TRUE, cex=.8)


temporalpredict<-predict(temporal,type = "class",newdata = testing)
historicalpredict<-predict(Historical,type = "class",newdata = testing)
spatialpredict<-predict(spatial,type = "class",newdata = testing)

allpredict<-predict(all,type = "class",newdata = testing)

temporalConf=confusionMatrix(temporalpredict,testing$NextNovel)
historicalConf=confusionMatrix(historicalpredict,testing$NextNovel)
spatialConf=confusionMatrix(spatialpredict,testing$NextNovel)

allConf=confusionMatrix(allpredict,testing$NextNovel)

