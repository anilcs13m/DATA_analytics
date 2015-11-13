#get working directory
getwd()
# load dataset 
letterFrame= read.csv("letters_ABPR.csv")
# summary/statics
summary(letterFrame)
# str
str(letterFrame)
# PREDICTING B OR NOT B
# add a new colunm to the dataframe names as isB for the letter to be B
letterFrame$isB = as.factor(letterFrame$letter == "B")
# load some required library
library(caTools)
library(rpart)
library(randomForest)

# Now split the data set into a training and testing set, putting 50% of the data in the training set
# set seed 1000
set.seed(1000)
# split dataset based on the colunm
spl = sample.split(letterFrame$isB, SplitRatio = 0.5)
# training and test data 
train = subset(letterFrame, spl == TRUE)
test = subset(letterFrame, spl == FALSE)
# accuracy of the baseline model
table(letterFrame$isB)
# calculation
1175/(1175+383)
# build a classification tree to predict whether a letter is a B or not, using the training set to build your model.
# before building this model remove the variable letter from the dataset as we are try to prediction 
# we are removing the letter as letter isB is the subset of the letter
CartModel = rpart(isB ~ . - letter, data=train, method="class")
# use default parameters in our CART model, so no need to add cp and minbucket parameters to our model
# use predict function to predict the accuracy of our model
pred = predict(CartModel, newdata=test, type="class") # we are using the class as we are predicting the class of character
# cnfusion metric
table(test$isB,pred)
# accuracy
(1118+340)/nrow(test)
# set seed for the new model randomForest model
set.seed(1000)
# build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set.
RFModel = randomForest(isB ~ . - letter, data=train)
# use predict function to get the new prediction of the model 
predRF = predict(RFModel,newdata = test)
# confusion matrix between the prediction of the model and the actual level of the letter
table(test$isB,predRF)
# accuracy
(1165+374)/nrow(test)
######
# PREDICTING THE LETTERS A, B, P, R
#####
# The variable in our data frame which we will be trying to predict is "letter". Start by converting letter in the original data set
# (letters) to a factor by running the following command in R
letterFrame$letter = as.factor(letterFrame$letter)
# seed
set.seed(2000)
# split the dataset 50% of data in training set
spl = sample.split(letterFrame$letter, SplitRatio = 0.5)
train1 = subset(letterFrame, spl == TRUE)
test1 = subset(letterFrame, spl == FALSE)
# accuracy of baseline model
table(train1$letter)
# now build a model that predict the letter using all the independent variable except the isB variable as we are trying to predict that
# variable use mothed class as we are using the classification
CARTletter = rpart(letter ~ . - isB, data=train1, method="class")
# predict the model
predictletter = predict(CARTletter, newdata = test1, type = "class")
# confusion metrix
table(test1$letter, predictletter)
(348+318+363+340)/nrow(test1)
##########
### building the random forest model using the same variable the used in the previos model cart model
# set seed
set.seed(1000)
RFLetter = randomForest(letter~.-isB, data = train1)
# make predictiong using the predict fuction
predictRF = predict(RFLetter, newdata= test1)
# confusion matrix
table(test1$letter, predictRF)
(390+380 +393+364)/nrow(test1)


















