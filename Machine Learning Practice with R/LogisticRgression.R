library(caTools)
NBAStats <- read.csv(file = 'nba.games.stats 3.csv') # Reading the data file
NBAStats

# Split Data
split <- sample.split(NBAStats, SplitRatio=0.8)
split
train <- subset(NBAStats, split=TRUE)
test <- subset(NBAStats, split=FALSE)

levels(NBAStats$WINorLOSS) <- c(1,0)

#Building binomail logistic regression
model<- glm(WINorLOSS~FieldGoals+X3PointShots+FreeThrows, data = train, family = "binomial")
summary(model)

#Predictions
res <- predict(model,train, type = "response")
res

res <- predict(model,test, type="response")
res


#Build confusion matrix
confmatrix <- table(Actual_Value=test$WINorLOSS, Predicted_Value= res>0.5)
confmatrix

