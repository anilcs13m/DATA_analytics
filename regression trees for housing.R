## Reading the dataset into R and making plots
boston = read.csv("boston.csv")
str(boston)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col="blue",pch=19) ## plotting the tracts that are close to the river
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red",pch=19) ## plotting MIT in the plot
summary(boston$NOX)
points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col="green",pch=19) ## plotting the areas which have an above average airpollution level 
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.20],boston$LAT[boston$MEDV>=21.20],col="red",pch=19) ## above average priced houses

## Building a linear regression model 
latlonlm = lm(MEDV~LAT+LON,data=boston)
summary(latlonlm)
latlonlm$fitted.values
points(boston$LAT[latlonlm$fitted.values>=12.2],boston$LON[latlonlm$fitted.values>=12.2],col="blue",pch="$") ## plotting the values in which the fitted values are more than 12.2

## REGRESSION TREES
library(rpart)
library(rpart.plot)
latlontree = rpart(MEDV~LAT+LON,data=boston)
prp(latlontree)
plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.20],boston$LAT[boston$MEDV>=21.20],col="red",pch=19)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>=12.2],boston$LAT[fittedvalues>=12.2],col="blue",pch="$")
latlontree = rpart(MEDV~LAT+LON,data=boston,minbucket=50)
plot(latlontree)
text(latlontree)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.20],boston$LAT[boston$MEDV>=21.20],col="red",pch=19)

## putting it all together
library(caTools)
set.seed(123)
split=sample.split(boston$MEDV,SplitRatio=0.7)
train = subset(boston,split==TRUE)
test = subset(boston,split==FALSE)
linreg = lm(MEDV~LON+LAT+MEDV+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
summary(linreg)
linear.pred = predict(linreg,newdata=test)
linear.sse = sum((linear.pred-test$MEDV)^2)
linear.sse
linregtree = rpart(MEDV~LON+LAT+MEDV+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
prp(linregtree)
tree.pred = predict(linregtree,newdata=test)
tree.sse = sum((tree.pred-test$MEDV)^2)
tree.sse

## cross validation
library(caret)
library(e1071)
tr.control=trainControl(method="cv",number=10)
cp.grid = expand.grid(.cp=(0:10)*0.001)
tr = train(MEDV~LON+LAT+MEDV+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train,method="rpart",trControl=tr.control,tuneGrid=cp.grid)
tr
best.tree = tr$finalModel
prp(best.tree)
best.tree.pred = predict(best.tree,newdata=test)
best.tree.sse = sum((best.tree.pred-test$MEDV)^2)
best.tree.sse
