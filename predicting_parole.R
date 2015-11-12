# get working directory
getwd()
# loading dataset
parole = read.csv("parole.csv")
# summary
summary(parole)
# nrow
nrow(parole)
str(parole)
# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)
# PREPARING THE DATASET
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state) 
summary(parole$crime)

# Using the as.factor() function, convert these variables to factors. Keep in mind that we are not changing the values, just the way R
# understands them (the values are still numbers)
# SPLITTING INTO A TRAINING AND TESTING SET
set.seed(144)
library(caTools)
# split ratio 70%
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split ==FALSE)
# summary
nrow(train)
nrow(test)
# SPLITTING INTO A TRAINING AND TESTING SET
# BUILDING A LOGISTIC REGRESSION MODEL
model = glm(violator~., data=train, family="binomial")
# summary
summary(model)
# If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit increase in
# the variable.
# If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the variable.
#######
# EVALUATING THE MODEL ON THE TESTING SET
######
predictions = predict(model, newdata=test, type="response")
# evaluate the model's predictions on the test set using a threshold of 0.5
table(test$violator, as.numeric(predictions >= 0.5))
# accuracy
(167+12)/202 = 0.886
# sensitivity
12/(11+12) = 0.522
# specificity
167/(167+12) = 0.933
# What is the accuracy of a simple model that predicts that every parolee is a non-violator
table(test$violator)
179/202 = 0.886
# EVALUATING THE MODEL ON THE TESTING SET
# ROCR and AUC value for model
library(ROCR)
pred = prediction(predictions, test$violator)
as.numeric(performance(pred, "auc")@y.values)
# meaning of AUC
# The AUC deals with differentiating between a randomly selected positive and negative example. 
# It is independent of the regression cutoff selected.
