#check working directory
getwd()
#load data
baseball = read.csv('baseball.csv')
summary(baseball)
str(baseball)
nrow(baseball)
#total number of years included in this dataset.
table(baseball$Year)
#count year
length(table(baseball$Year)) 
#analyzing teams that made the playoffs use subset
baseball = subset(baseball, Playoffs == 1)
str(baseball)
nrow(baseball)
#number of teams making the playoffs in some season?
table(baseball$Year)
table(table(baseball$Year))

PlayoffTable = table(baseball$Year)
names(PlayoffTable)
str(names(PlayoffTable))
#number of teams in the playoffs for each team/year pair
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] 

table(baseball$NumCompetitors) 
#Bivariate Models for Predicting World Series Winner
#Add a variable named WorldSeries to the baseball data frame
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries) 
#Use the entire dataset baseball to build the models
model = glm(WorldSeries~Year, data=baseball, family="binomial")
summary(model)

model_new = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
summary(model_new)
# Which of the following variable pairs have a high degree of correlation 
cor(baseball[c(“Year”, “RA”, “RankSeason”, “NumCompetitors”)])
#Build all six of the two variable models
#with two year and RA
Model1 = glm(WorldSeries ~ Year + RA, data=baseball, family=binomial)
#year and rankseason
Model2 = glm(WorldSeries ~ Year + RankSeason, data=baseball, family=binomial)
#year numcometitors
Model3 = glm(WorldSeries ~ Year + NumCompetitors, data=baseball, family=binomial)
# RA and RankSeason
Model4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)
# RA and Numpetitions
Model5 = glm(WorldSeries ~ RA + NumCompetitors, data=baseball, family=binomial)
#rankseason and numcompetitors
Model6 = glm(WorldSeries ~ RankSeason + NumCompetitors, data=baseball, family=binomial)
#summary of all
summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)
summary(Model5)
summary(Model6)
