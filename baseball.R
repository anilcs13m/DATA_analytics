# getwd working directory
getwd()
# load data set
baseball = read.csv("baseball.csv")
# summary statics of data set
summary(baseball)
str(baseball) 

# LIMITING TO TEAMS MAKING THE PLAYOFFS as PER YEAR
table(baseball$Year)

# LIMITING TO TEAMS MAKING THE PLAYOFFS
# TAKE SUBSET OF DATASET ACCORDING TO THE PLAYOFFS
baseball = subset(baseball, Playoffs == 1)
# TOTAL NUMBER OF OBSERBESION
str(baseball)

# LIMITING TO TEAMS MAKING THE PLAYOFFS
table(baseball$Year)

# ADDING AN IMPORTANT PREDICTOR BASEL
PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)

# ADDING AN IMPORTANT PREDICTOR
PlayoffTable[c("1990", "2001")]

# ADDING AN IMPORTANT PREDICTOR
PlayoffTable[as.character(baseball$Year)]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

# ADDING AN IMPORTANT PREDICTOR
str(baseball)
str(subset(baseball, NumCompetitors == 8))

# BIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
str(baseball)
table(baseball$WorldSeries)

# BIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
model1 <- glm(WorldSeries ~ Year, data = baseball, family = "binomial")
model2 <- glm(WorldSeries ~ RS, data = baseball, family = "binomial")
model3 <- glm(WorldSeries ~ RA, data = baseball, family = "binomial")
model4 <- glm(WorldSeries ~ W, data = baseball, family = "binomial")
model5 <- glm(WorldSeries ~ OBP, data = baseball, family = "binomial")
model6 <- glm(WorldSeries ~ SLG, data = baseball, family = "binomial")
model7 <- glm(WorldSeries ~ BA, data = baseball, family = "binomial")
model8 <- glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial")
model9 <- glm(WorldSeries ~ OOBP, data = baseball, family = "binomial")
model10 <- glm(WorldSeries ~ OSLG, data = baseball, family = "binomial")
model11 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
model12 <- glm(WorldSeries ~ League, data = baseball, family = "binomial")
# SUMMARY OF ALL MODEL
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)

# MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
m <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(m)

# MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

# MULTIVARIATE MODELS FOR PREDICTING WORLD SERIES WINNER
aic <- rep(0, 10) # repeat 0 ten times to store AIC

m1 <- glm(WorldSeries ~ Year, data = baseball, family = "binomial")
aic[1] = summary(m1)$aic

m2 <- glm(WorldSeries ~ RA, data = baseball, family = "binomial")
aic[2] = summary(m2)$aic

m3 <- glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial")
aic[3] = summary(m3)$aic

m4 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
aic[4] = summary(m4)$aic

m5 <- glm(WorldSeries ~ Year + RA, data = baseball, family = "binomial")
aic[5] = summary(m5)$aic

m6 <- glm(WorldSeries ~ Year + RankSeason, data = baseball, family = "binomial")
aic[6] = summary(m6)$aic

m7 <- glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = "binomial")
aic[7] = summary(m7)$aic

m8 <- glm(WorldSeries ~ RA + RankSeason, data = baseball, family = "binomial")
aic[8] = summary(m8)$aic

m9 <- glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = "binomial")
aic[9] = summary(m9)$aic

m10 <- glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = "binomial")
aic[10] = summary(m10)$aic
aic
which.min(aic)