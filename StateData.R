# check current working directory
getwd()
# load csv file
statedata = read.csv("statedata.csv")
# Structure of the dataset
str(statedata)
# Statistical summary
summary(statedata)
#name of all the variable use
names(statedata)
# Plot all of the states' centers with latitude on the y (as a y-axis) and  and longitude on the x (as a x-axis)
Plot(statedata$x,statedata$y) 
# determine which region of the US (West, North Central, South, or Northeast) 
# has the highest average high school graduation rate of all the states in the region
tapply(statedata$HS.Grad, statedata$state.region, mean)
# max value
max(tapply(statedata$HS.Grad, statedata$state.region, mean))
# box plot muder rate by region
boxplot(statedata$Murder ~ statedata$state.region)
# Predicting Life Expectancy
# build a linear model using lm command in R
model = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data=statedata)
#sumary of this model to find the coefficent of variables
summary(model)
# Now plot a graph of life expectancy vs. income using the command:
plot(statedata$Income, statedata$Life.Exp)
# remove some insignificant variables
model1 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
# summary
summary(model1)
# predictions
sort(predict(model))
# predictions by new model
sort(predict(model1))
# state actually has the lowest life expectancy
which.min(statedata$Life.Exp)
# 40
statedata$state.name[40] # name of the state
# residuals (the difference between the predicted and actual values)
sort(abs(model$residuals))
sort(abs(statedata$Life.Exp - predict(model)))
