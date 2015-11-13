statedata = read.csv("statedataSimple.csv")
str(statedata)

## Building the linear regression model

linreg = lm(Life.Exp~.,data=statedata)
summary(linreg)
Pred = predict(linreg)

## Calculating the sum of squared error for this model

SSE = sum((Pred-statedata$Life.Exp)^2)
SSE

## Building a linear regression model with less independent variables

linreg2 = lm(Life.Exp~Population+Murder+Frost+HS.Grad,data=statedata )
summary(linreg2)
Pred2 = predict(linreg2)
SSE2 = sum((Pred2-statedata$Life.Exp)^2)
SSE2

## Building a CART model
library(rpart)
library(rpart.plot)
CARTmodel=rpart(Life.Exp~.,data=statedata)
prp(CARTmodel)
Pred3 = predict(CARTmodel)
SSE3 = sum((Pred3-statedata$Life.Exp)^2)
SSE3

## Rebuilding the model
CARTmodel2=rpart(Life.Exp~.,data=statedata,minbucket=5)
prp(CARTmodel2)
Pred4 = predict(CARTmodel2)
SSE4 = sum((Pred4-statedata$Life.Exp)^2)
SSE4

## Building a CART model with only 1 independent variable
CARTmodel3=rpart(Life.Exp~Area,data=statedata,minbucket=1)
prp(CARTmodel3)
Pred5 = predict(CARTmodel3)
SSE5 = sum((Pred5-statedata$Life.Exp)^2)
SSE5

## Performing cross validation
library(caret)
library(e1071)
set.seed(111)
numFolds = trainControl(method="cv",number=10)
cpGrid = expand.grid(.cp=seq( 0.01,0.50,0.01))
train(Life.Exp~.,data=statedata,method="rpart",trControl=numFolds,tuneGrid=cpGrid)

## Building the cart model with the new cp value
CARTmodel4=rpart(Life.Exp~.,data=statedata,cp=0.12)
prp(CARTmodel4)
pred6 = predict(CARTmodel4)
SSE6 = sum((pred6-statedata$Life.Exp)^2)
SSE6

## Performing cross validation again
set.seed(111)
numFolds = trainControl(method="cv",number=10)
cpGrid = expand.grid(.cp=seq( 0.01,0.50,0.01))
train(Life.Exp~Area,data=statedata,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
CARTmodel5 = rpart(Life.Exp~Area,data=statedata,cp=0.02)
prp(CARTmodel5)
pred7 = predict(CARTmodel5)
SSE7 = sum((pred7-statedata$Life.Exp)^2)
SSE7
