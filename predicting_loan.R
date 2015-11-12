# check cuurent working directory
getwd()
#load data
loans = read.csv("loans.csv")
#str(loans)
str(loans)
#summary
summary(loans)
# What proportion of the loans in the dataset were not paid in full?
table(loans$not.fully.paid)
#subset data based on the missing values
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) |is.na(delinq.2yrs) | is.na(pub.rec))
#analysis this missing values summary and str
#repairing the dataset
#fill the missing values with imputation

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#split the dataset in training and testing data set with a random seed 144
# 70% is the dataset 
# make a logistic regration model and use summary function 
# which are the significate variable use in the model
# Significant variables have at least one star, or a Pr(>|z|) value less than 0.05.
# for spliting load library caTools
library(caTools)
set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
#The model can be trained and summarized with the following commands:
mod = glm(not.fully.paid~., data=train, family="binomial")
#use summary to check significate variable
summary(mod)
# prediction model
# Predict the probability of the test set loans not being paid back in full (remember type="response" for the predict function).
# Store these predicted probabilities in a variable named predicted.risk and add it to your test set (we will use this
# variable in later parts of the problem). Compute the confusion matrix using a threshold of 0.5.
test$predicted.risk = predict(mod, newdata=test, type="response")

table(test$not.fully.paid, test$predicted.risk > 0.5)
#Use the ROCR package to compute the test set AUC.
library(ROCR)
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate)
#What is the test set AUC of the bivariate model?
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)
# COMPUTING THE PROFITABILITY OF AN INVESTMENT




