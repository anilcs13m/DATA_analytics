# get working directory
getwd()
# load dataset
gerber = read.csv("gerber.csv")
# summary
summary(gerber)
# str
str(gerber)
# What proportion of people in this dataset voted in this election?
table(gerber$voting)
108696/(108696+235388)
# EXPLORATION AND LOGISTIC REGRESSION
# largest percent of people who voted from these group
# civicduty, hawthorne, self, neighbors
# for that use tapply function
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
# Build a logistic regression model for voting using the four treatment group variables as the independent variables
# (civicduty, hawthorne, self, and neighbors)
Model = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
# summary
summary(Model)
# threshold 0.3 what is the accuracy of the model
pred = predict(Model, type="response")
# confusion matrix:
table(gerber$voting, pred > 0.3)
(134513+51966)/(134513+100875+56730+51966)
# use threshold 0.5
table(gerber$voting, pred > 0.5)
(235388+0)/(235388+108696)

library(ROCR)
# Load CART packages
library(rpart)
library(rpart.plot)

ROCRpred = prediction(pred, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

###########
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary
# importance to the treatment group.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
# INTERACTION TERMS
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
prp(CARTsex, digits=6)
# create a model using "sex" and "control"
LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
predict(LogModel2, newdata=Possibilities, type="response")







