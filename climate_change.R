#get wrking directory
getwd()
#load data set 
climate = read.csv("climate_change.csv")
#summary of data
summary(climate)
#structure of data
str(climate)
# split the data into a training set, consisting of all the observations up to and including 2006, and a testing set consisting
# of the remaining years (hint: use subset). A training set refers to the data that will be used to build the model (this is the
# data we give to the lm() function), and a testing set refers to the data we will use to test our predictive ability

train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)
# build a linear regression model to predict the dependent variable Temp, using MEI, CO2, CH4, N2O, CFC.11, CFC.12,TSI, 
# and Aerosols as independent variables (Year and Month should NOT be used in the model). Use the training set to build the model

model = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
#summary of model
summary(model)
#analusis the value of R^2
#multiple R-Square value
#We will consider a variable signficant only if the p-value is below 0.05.

# UNDERSTANDING THE MODEL
# Compute the correlations between all the variables in the training set

# PROBLEM 2 - UNDERSTANDING THE MODEL  
# Current scientific opinion is that nitrous oxide and CFC-11 are greenhouse gases: gases that are able to trap
# heat from the sun and contribute to the heating of the Earth. However, the regression coefficients of both 
# the N2O and CFC-11 variables are negative, indicating that increasing atmospheric concentrations of either 
# of these two compounds is associated with lower global temperatures.
# Which of the following is the simplest correct explanation for this contradiction?
# Find the correct answer by spotting the wrong answers.
cor(train)
# Compute the correlations between all the variables in the training set. Which of the following independent 
# variables is N2O highly correlated with (absolute correlation greater than 0.7)? Select all that apply.
# Answer based on the function above
# Which of the following independent variables is CFC.11 highly correlated with? Select all that apply.
# Answer based on the function above
# Given that the correlations are so high, let us focus on the N2O variable and build a model with only MEI, TSI, Aerosols and
# N2O as independent variables. Remember to use the training set to build the model.

model1 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
#sumary
summary(model1)

#AUTOMATICALLY BUILDING THE MODEL

# HINT: If your initial full model was called "climateLM", you
# could create a new model with the step function by typing step(climateLM).

newModel = step(model)

#TESTING ON UNSEEN DATA 

Predict = predict(newModel, newdata = test)
#SEE SST and R^2
SSE = sum((Predict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST













