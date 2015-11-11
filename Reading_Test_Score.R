#set path
setwd('/home/anil/ML_NLP_projects/analyticsR')
#get current path
getwd()
#load data
test = read.csv('pisa2009test.csv')
train = read.csv('pisa2009train.csv')
#check summary statics
summary(test)
summary(train)
#structure
str(test)
str(train)
# what is the average reading test score of males  in training data
tapply(train$readingScore,train$male,mean)
#what is the average reading test score for females in training data use 0 and 1
nrow(train)
nrow(test)
#remove missing values na.omit is use for this
test = na.omit(test)
train = na.omit(train)
nrow(test)
nrow(train)
# see the difference between the two

# FACTOR VARIABLES  
# Which of the following variables is an unordered factor with at least 3 levels? (Select all that apply.)
# raceeth
# Which of the following variables is an ordered factor with at least 3 levels? (Select all that apply.)
# grade
# Consider the variable "raceeth" in our problem, we will select White as the reference level.
# Which binary variables will be included in the regression model? (Select all that apply.)
# All the variables but not "White"
##########################
train$raceeth = relevel(train$raceeth,"White")
test$raceeth = relevel(test$raceeth,"White")
#build a linear regression model (call it lmScore) using the training set to predict readingScore using all the remaining variables
lmScore <- lm(readingScore ~., data=train)
# What is the Multiple R-squared value of lmScore on the training set?
summary(lmScore)
# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = (lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(train))
# or
RMSE = sqrt(mean(lmScore$residuals^2))
# Consider two students A and B. They have all variable values the same, except that student A is in grade 
# 11 and student B is in grade 9. What is the predicted reading score of student A minus the predicted 
# reading score of student B?
summary(lmScore)
#COMPARING PREDICTIONS FOR SIMILAR STUDENTS

#PREDICTING ON UNSEEN DATA 
predTest = predict(lmScore, newdata=test)
#summary
summary(predTest)
#TEST SET SSE AND RMSE
SSE = sum((predTest-test$readingScore)^2) #SSE
#RMSE
RMSE = sqrt(mean((predTest-test$readingScore)^2))
#BASELINE PREDICTION AND TEST-SET SSE
baseline = mean(train$readingScore) #baseline model the total sum of squares (SST).
#What is the sum of squared errors of the baseline model on the testing set?
SST = sum((baseline-test$readingScore)^2)
#R - squared 1 - SSE/SST
# where SSE is the sum of squared errors of the model(sum((predTest-test$readingScore)^2)) on the test set and SST is
# the sum of squared errors of the baseline model (mean(train$readingScore)).
###################
1 - SSE/SST
###





