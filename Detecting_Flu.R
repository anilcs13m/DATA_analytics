# DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA
# get working directory
getwd()
#load data set
FluTrain = read.csv("FluTrain.csv")
#summary
summary(FluTrain)

# which week corresponds to the highest percentage of ILI-related physician visits? 
# Select the day of the month corresponding to the start of this week.

FluTrain = subset(FluTrain, ILI == max(ILI))
#or
which.max(FluTrain$ILI)
#303
FluTrain$Week[303]
# Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain = subset(FluTrain, Queries ==max(Queries))
#or
which.max(FluTrain$Queries)
#303
FluTrain$Week[303]
# UNDERSTANDING THE DATA
# What best describes the distribution of values of ILI?
# use histogram of ILI variable
hist(FluTrain$ILI) # and see data is skew right
# When handling a skewed dependent variable, it is often useful to predict the logarithm of the dependent variable instead of
# the dependent variable itself -- this prevents the small number of unusually large or small observations from having an
# undue influence on the sum of squared errors of predictive models. In this problem, we will predict the natural log of the ILI
# variable, which can be computed in R using the log() function
#######
#Plot the natural logarithm of ILI versus Queries
plot(FluTrain$Queries, log(FluTrain$ILI))

# LINEAR REGRESSION MODEL
# Let's call the regression model from the previous problem (Problem 2.1) FluTrend1 and run it in R. Hint: to take the
# logarithm of a variable Var in a regression equation, you simply use log(Var) when specifying the formula to the lm()
# function

FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)
#see value of R^2

# For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation
# between the independent and the dependent variables. What is the relationship we infer from our problem?
#####
# R-squared = correlation^2
#####
# we first need to compute the correlation between the independent variable used in the model (Queries) and 
# the dependent variable (log(ILI))
Correlation = cor(FluTrain$Queries, log(FluTrain$ILI))
# it seem that correlation^2 = R-squared value
############
### PERFORMANCE ON THE TEST SET
############
FluTest = read.csv("FluTest.csv")
#summary
summary(FluTest)
# predict the performance of our model FluTrend1
PredTest1 = predict(FluTrend1, newdata=FluTest)

# However, the dependent variable in our model is log(ILI), so PredTest1 would contain predictions of the log(ILI) value. We
# are instead interested in obtaining predictions of the ILI value. We can convert from predictions of log(ILI) to predictions of
# ILI via exponentiation, or the exp() function. The new code, which predicts the ILI value
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? 

which(FluTest$Week == "2012-03-11 - 2012-03-17")
# prediction number 11
PredTest1[11]
# PERFORMANCE ON THE TEST SET

# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
#############
####
#### (Observed ILI - Estimated ILI)/Observed ILI
####
############
# PERFORMANCE ON THE TEST SET
# Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of 
# ILI-related physician visits
SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
#or
sqrt(mean((PredTest1-FluTest$ILI)^2))
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
#How many values are missing in the new ILILag2 variable?
summary(FluTrain$ILILag2)
# TRAINING A TIME SERIES MODEL
# Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these
# two variables?
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as
# well as the log of the ILILag2 variable
FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
# summary
summary(FluTrend2)
# EVALUATING THE TIME SERIES MODEL IN THE TEST SET
# We can add the new variable with:
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

# Fill in the missing values for ILILag2 in FluTest. In terms of syntax, you could set the value of ILILag2 in row "x" of the FluTest
# data frame to the value of ILI in row "y" of the FluTrain data frame with "FluTest$ILILag2[x] = FluTrain$ILI[y]". Use the
# answer to the previous questions to determine the appropriate values of "x" and "y". It may be helpful to check the total
# number of rows in FluTrain using str(FluTrain) or nrow(FluTrain).
nrow(FluTrain)
# this shows us 417 rows
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
# What is the new value of the ILILag2 variable in the first row of FluTest?
FluTest$ILILag2[1]
# What is the new value of the ILILag2 variable in the second row of FluTest?
FluTest$ILILag2[2]
# Obtain test set predictions of the ILI variable from the FluTrend2 model, again remembering to call the exp() function on
# the result of the predict() function to obtain predictions for ILI instead of log(ILI).
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
#or
sqrt(mean((PredTest2-FluTest$ILI)^2))




















