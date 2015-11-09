# get working directory
getwd()
# load data
elantra<-read.csv("elantra.csv")
#summary
summary(elantra)
# structure
str(elantra)
#Split the data set into training and testing sets as follows:
# place all observations for 2012 and earlier in the training set, and all observations for 2013 and 2014 into the testing set
#train
Train = subset(elantra, Year <= 2012)
#test
Test = subset(elantra, Year > 2012) 
#number of observation
str(Test)
str(Train)
#or
nrow(Test)
nrow(Train)
#linear model 
model = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all, data=Train)
#summary
summary(model)
# improve the model
model1= lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data=Train)
#summary
summary(model1)
#Re-run the regression with the Month variable modeled as a factor variable. 
#(Create a new variable that models the Month as a factor (using the as.factor function) instead of 
#overwriting the current Month variable.We'll still use the numeric version of Month later in the problem.)
Train$MonthFactor = as.factor(Train$Month)

Test$MonthFactor = as.factor(Test$Month)
#new model
model2 = lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data=Train)
#summary
summary(model2)
# compute the correlations of the variables in the training set.
cor(Train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

#obtain predictions on the test set by using the predict function:
PredictTest = predict(model, newdata=Test)
#sum of squared errors of the model on the test set?
SSE = sum((PredictTest - Test$ElantraSales)^2)
#What is the test set R-Squared
SST = sum((mean(Train$ElantraSales) - Test$ElantraSales)^2)
#What is the largest absolute error that we make in our test set predictions?
max(abs(PredictTest - Test$ElantraSales))
#In which period (Month,Year pair) do we make the largest absolute error in our prediction?
which.max(abs(PredictTest - Test$ElantraSales))
