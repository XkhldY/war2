require("ggmap")
require(data.table)
require(plyr)
require(lubridate)
library(leaflet)
require(foreach)
library(Rcpp)
library(doParallel)
# library("fpc")
library(dbscan)
library("geosphere")
training<-fread("D:\\training.csv")

testing<-fread("D:\\testing.csv")


training$NoOfDays=log10(training$NoOfDays)
training$distinct=log10(training$distinct)
training$distdiff=log10(training$distdiff)

d<-glm(label~timediff,family = binomial("logit"), data = training)

c<-predict(d,type = "response",newdata = testing)

# confint(d)
f1 = roc(label~timediff, data=training)
confusionMatrix(data=c, testing$label)
# accuracy <- table(d$data, testing$label)


d<-glm(formula = label ~ 
         # preNovel+
         # NoOfDays+
         # Nratio+
         # distinct+
         # distdiff+
         # VisitingRatio+
         timediff+
         HOW+
         DOW+
         HOD, 
       family = binomial("logit"), data = training)



library(caret)
varImp(d)


testing1=subset(testing,select =c(
                                  "timediff",
                                  "HOW",
                                  "DOW",
                                  "HOD" ) )
pred = predict(d, newdata=testing1)


