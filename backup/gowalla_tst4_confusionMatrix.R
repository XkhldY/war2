library(rpart)
library(pROC)

training<-fread("D:\\training.csv")
testing<-fread("D:\\testing.csv")
t<-training#[1:10000,]
t$label<-as.character(t$label)
t$HOW<-log10(t$HOW)
temporal<-rpart(label~HOW+timediff+DOW+HOD,method = "class",data = t)
Historical<-rpart(label~Nratio+NoOfDays+preNovel+distinct,method = "class",data = t)
spatial<-rpart(label~distdiff+VisitingRatio,method = "class",data = t)

all<-rpart(label~HOW+timediff+DOW+HOD
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

temporalConf=confusionMatrix(temporalpredict,testing$label)
historicalConf=confusionMatrix(historicalpredict,testing$label)
spatialConf=confusionMatrix(spatialpredict,testing$label)

allConf=confusionMatrix(allpredict,testing$label)

