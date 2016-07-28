# Load the required libraries.
library(xgboost)
library(caret)      # for confusionMatrix
library(data.table)

training100<-fread("D:\\training_100.csv")
testing100<-fread("D:\\testing_100.csv")

#Check the data structure
# data(iris)
# print(str(iris))
# 
# #Split the iris data into training (70%) and testing(30%).
# set.seed(100)
# ind = sample(nrow(iris),nrow(iris)* 0.7)
# training = iris[ind,]
# testing = iris[-ind,]

#Set the parameters for cross-validation and xgboost.
#Note: This is a multi-class classification problem, and the evaluation metric is "mlogloss".
#      The same parameters are used by Step 1 and Step 2.
#      You can try different values for nthread, max_depth, eta, gamma, etc., and see if you get lower prediction error.

param       = list("eta"=0.1, "max_depth"=6, "min_child_weight"=100,
                   "objective"="binary:logistic", "eval_metric"="rmse")

#Identify the Predictors and the dependent variable, aka label.
# NextNovel~
train=subset(x = training100,select = c("HOW","timediff","DOW","HOD",
                                        "distinct","preNovel","Nratio",
                                        "distdiff","VisitingRatio",
                                        "NoOfDays","label"))

test=subset(x = testing100,select = c("HOW","timediff","DOW","HOD",
                                       "distinct","preNovel","Nratio",
                                       "distdiff","VisitingRatio",
                                       "NoOfDays","label"))
train[]<- lapply(train, function(x) as.numeric(x))
test[]<- lapply(test, function(x) as.numeric(x))

str(train)

set.seed(100)
a=as.matrix(train)
b=as.numeric(training100$NextNovel)
c=as.matrix(b)
dtrain <- xgb.DMatrix(as.matrix(test))

# cv.nround = 200;  # Number of rounds. This can be set to a lower or higher value, if you wish, example: 150 or 250 or 300  
# bst.cv = xgb.cv(
#   param=param,
#   data = as.matrix(a),
#   label = b,
#   nfold = 3,
#   nrounds=cv.nround,
#   prediction=T)

bst = xgboost(
  param=param,
  data =a,
  label = b,
  nrounds=20)

# Make prediction on the testing data.
test$predictions = predict(bst, as.matrix(test))
prediction <- as.numeric(test$predictions > 0.5)

#Translate the prediction to the original class or Species.
# testing$prediction = ifelse(testing$prediction==0,"setosa",ifelse(testing$prediction==1,"versicolor","virginica"))

#Compute the accuracy of predictions.
confusionMatrix( prediction,testing100$NextNovel)

# err <- mean(as.numeric(test$predictions > 0.5) != testing100$NextNovel)
# print(paste("test-error=", err))


importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix)



#################################################################################################################################
#Extra: Use some other model for the same prediction.
#       (randomForest with cross-validation using the caret package)
# 
# set.seed(100)
# train_control = trainControl(method="cv",number=10)
# model.rf = train(Species~., data=training, trControl=train_control, method="rf")
# 
# testing$prediction.rf = predict(model.rf,testing[,predictors])
# 
# #Compute the accuracy of predictions.
# confusionMatrix( testing$prediction.rf,testing$Species)
################################################################################################################################
####################################