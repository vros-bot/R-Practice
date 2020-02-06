#Decision Tree

#import libraries
install.packages("rpart")
library(rpart)

install.packages("party")
library(party)

#to invoke the external dataset in NBAStats variable
NBAStats <- read.csv(file = 'nba.games.stats.csv') 
NBAStats

View(NBAStats)

#split the data by team
team <- split(NBAStats, NBAStats$Team)
team

#Boston team data
BostonStat <- team$BOS
BostonStat

set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(BostonStat), alpha * nrow(BostonStat))
train.set <- BostonStat[inTrain,]
test.set  <- BostonStat[-inTrain,]


# Decision Tree
BosTree <- ctree(TeamPoints~ X3PointShots+FreeThrows+Assists, data = train.set)
plot(BosTree)

#Predictions
predict(BosTree, test.set, type="prob")
