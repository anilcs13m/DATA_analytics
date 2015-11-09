#get the current working dir
getwd()
#read the data in data frame IPP
IPP<-read.csv("AnonymityPoll.csv")
# structure
str(IPP)
# summary statics
summary(IPP)
#name of all the column
names(IPP)

#The number of people who took the poll is equal to the number of rows of the data frame, 
# and can be obtained with nrow(poll) or from the output of str(poll)
nrow(IPP)
# number of people with smartphones using the table() and summary() commands on the Smartphone variable
summary(IPP$Smartphone)
# using table
table(IPP$Smartphone)
# suppose we want to create a table of the variables "Sex" and "Region". We would type 
table(IPP$Sex, IPP$Region)

# Which of the following are states in the Midwest census region
MidwestInterviewees = subset(IPP, Region=="Midwest")
table(MidwestInterviewees$State)
#  number of interviewees from each South region state 
SouthInterviewees = subset(IPP, Region=="South")
table(SouthInterviewees$State)

# find the relation between the user of internet and smartphone 0 for not and 1 present
# first represent rows and second represent column
table(IPP$Internet.Use, IPP$Smartphone)
# missing value for their smartphone use and interner use
summary(IPP$Intenet.Use)
summary(IPP$Smartphone)
# create a data frame limit that having smartphone user of internet user
# user subset to create a subset of data frame
limited = subset(IPP, Internet.Use == 1 | Smartphone == 1)
table(limited$Tried.Masking.Identity)
# plot age vs Info.On.Internet
plot(limited$Age, limited$Info.On.Internet)
# What is the largest number of interviewees that have exactly the same
#  value in their Age variable AND the same value in their Info.On.Internet variable
max(table(limited$Age, limited$Info.On.Internet))
# jitter adds or subtracts a small amount of random noise to the values passed to it, and two runs will yield different results
jitter(c(4, 7, 8))
# we can use jitter with some variable to add some noise in the data and plot
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
# Use the tapply() function to obtain the summary of the Info.On.Internet value, 
# broken down by whether an interviewee is a smartphone user
tapply(limited$Info.On.Internet, limited$Smartphone, summary)
#  tapply to break down the Tried.Masking.Identity variable for smartphone and non-smartphone users.
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)


