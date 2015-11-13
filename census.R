# get the working directory
getwd()
# load the dataset
census = read.csv("census.csv")
# statistic 
summary(census)
str(census)
# A LOGISTIC REGRESSION MODEL
#####
# split the dataset into training and testing set 60% and 40%
#####
# build a logistic regration model to predict the over50k income
library(caTools)
set.seed(144)
#########
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)
########
# logistic regression model use glm function
GLModel= glm( over50k ~ . , family="binomial", data = train)
# summary
summary(GLModel)
# predict the accuracy of the model Use a threshold of 0.5
GLMPred = predict(GLModel,newdata = test,type = "response")
# confusion matrix
table(test$over50k, GLMPred >= 0.5)
# baseline accuracy
table(test$over50k)
# AUC for this model
library(ROCR)
ROCRpred = prediction(GLMPred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# A CART MODEL
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
# model with default parameter
treeModel = rpart( over50k ~ . , method="class", data = train)
# Plot the tree:
prp(treeModel)
# accuracy of model using the threshold of 0.5
pred = predict(treeModel, newdata = test, type = "class")
# confusion matrix
table(test$over50k, pred)
(9243+1596)/(9243+470+1482+1596) 

predictTest = predict(treeModel, newdata = test)
# take second column
predictTest = predictTest[,2]
# AUC
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
# A RANDOM FOREST MODEL
# set sedd
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
# build a random forest model to predict "over50k", using the dataset "trainSmall"
# use threshold 0.5
rfModel = randomForest(over50k ~ . , data = trainSmall)
# predict 
predictTest = predict(rfModel, newdata=test)
# confusion matrix
table(test$over50k, predictTest)
(9614+1050)/nrow(test) 

#vu = varUsed(MODEL, count=TRUE)
vu = varUsed(rfModel, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rfModel$forest$xlevels[vusorted$ix]))
######
varImpPlot(rfModel)
######
# SELECTING CP BY CROSS-VALIDATION
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
# SELECTING CP BY CROSS-VALIDATION
model = rpart(over50k~., data=train, method="class", cp=0.002)
# make prediction
predictTest = predict(model, newdata=test, type="class")
table(test$over50k, predictTest)
(9178+1838)/(9178+535+1240+1838)

prp(predictTest)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))











