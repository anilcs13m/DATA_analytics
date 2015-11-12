#get working directoyr
getwd()
#load data
songs = read.csv("songs.csv")
#summary(songs)
# How many observations (songs) are from the year 2010?
# for this make table
table(songs$year)
# How many songs does the dataset include for which the artist name is "Michael Jackson"?
# for this subset that make subset of data frame for Michael Jackson
MJ = subset(songs, artistname == "Michael Jackson")
# nrow
nrow(MJ)
str(MJ)
# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
MJ[c(“songtitle”, “Top10”)]

# Which timesignature value is the most frequent among songs in our dataset?
table(songs$timesignature)
#  the song with the highest tempo
which.max(songs$tempo)
# 6206
songs$songtitle[6206]
# CREATING OUR PREDICTION MODEL
#####
# We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split the data
# into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set
# "SongsTest", consisting of the 2010 song releases.
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)

nrow(SongsTest)
nrow(SongsTrain)
# create a predictive model for songs is in top10 or not scince outcome is binary value so we are use logistic regration model
# for this model we remove some of the variable that are not required in our model like song title, so we have to remove such variable 
# let make a vector of variable which we wants to remove
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
# To remove these variables from your training and testing sets
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
# use the glm function to build a logistic regression model to predict Top10
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
#summary()
summary(SongsLog1)
# BEWARE OF MULTICOLLINEARITY ISSUES!
# cor()
# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$loudness, SongsTrain$energy)

# Given that these two variables are highly correlated, Model 1 suffers from multicollinearity. To avoid this issue, we will omit
# one of these two variables and rerun the logistic regression. In the rest of this problem, we'll build two variations of our
# original model: Model 2, in which we keep "energy" and omit "loudness", and Model 3, in which we keep "loudness" and
# omit "energy"
# remove loudness
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
# remove energy
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)

# PROBLEM 4 - VALIDATING OUR MODEL
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45? 
predictMod3 <- predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictMod3 >= 0.45)
(309+19)/(309+19+40+5)
# What would the accuracy of the baseline model be on the test set? 
table(SongsTest$Top10)
314/(314+59)
# How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set),
# using a threshold of 0.45?
table(SongsTest$Top10, predictMod3 >= 0.45)
# How many non-hit songs does Model 3 predict will be Top 10 hits (again, looking at the test set), using a threshold of 0.45?
table(SongsTest$Top10, predictMod3 >= 0.45)
# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(40+19)
# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309+5)
# What conclusions can you make about our model?
# Apply logic to answer the question