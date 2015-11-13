## READING THE DATASET AND SPLITTING THE DATASET INTO TESTING AND TRAINING SETS
stevens = read.csv("stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio=0.7)
train = subset(stevens,spl==TRUE)
test = subset(stevens,spl==FALSE)

## BUILDING MODEL USING CART
StevensTree = rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="class",minbucket=25) ## this builds a classification tree
prp(StevensTree) ## Plots the tree in the graphics device
## minbucket is too small so split is large and overfitting occurs
## minbucket is large then the model becomes too simple and this reduces our models accuracy

## TESTING CART MODEL ON THE TEST SET AND CALCULATING THE ACCURACY
predictCART = predict(StevensTree,newdata=test,type="class")
table(test$Reverse,predictCART)
accuracy= (41+71)/(41+71+22+36)
accuracy
## 0.659

##  BUILDING THE ROC CURVE AND CALCULATING THE AUC VALUE
PredROC = predict(StevensTree,newdata=test)
pred = prediction(PredROC[,2],test$Reverse)
perf = performance(pred,"tpr","fpr")
plot(perf)
AUC = as.numeric(performance(pred,"auc")@y.values)
AUC
## 0.6927105
## INTRODUCTION TO RANDOM FORESTS

## BUILDING THE MODEL WITH RANDOM FORESTS AND TESTING ACCURACY
install.packages("randomForest")
library(randomForest)
StevensForest =  randomForest(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,nodesize=25,ntree=200)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
predict_forest = predict(StevensForest,newdata=test)
table(test$Reverse,predict_forest)
acc = (42+75)/(42+75+35+18)
acc

## PARAMETER VALUE SELECTION IN RANDOM FORESTS
## We use a method called K fold cross validation 
## Given training set, split into k pieces (here k = 5)
## Use k-1 folds to estimate a model, and test model on remaining one fold (“validation set”) for each candidate parameter value
## Repeat for each of the k folds
## smaller cp can lead to overfitting and bigger underfitting

## PERFOMING CROSS VALIDATION :
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)
## defining the no of folds and finding the cp value
numFolds = trainControl(method="cv",number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
## building the model with the new cp value and testing accuracy
StevensTreeCV = rpart(Reverse~ Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method="class",cp=0.19)
PredictCV = predict(StevensTreeCV , newdata=test,type="class")
table(test$Reverse,PredictCV)
(59+64)/(59+64+29+18)
prp(StevensTreeCV)

