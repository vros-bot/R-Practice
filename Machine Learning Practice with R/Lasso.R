library(glmnet)
library(lars)
library(MASS)
library(biglasso)

#Splitting the data
set.seed(1) 
row.number <- sample(x=1:nrow(Boston), size=0.7*nrow(Boston))
train = Boston[row.number,]
test = Boston[-row.number,]

x <- model.matrix(medv~., train)[,2:12]
y <-train$medv

#LASSO Regression
lasso.mod <- glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha=1, nlambda=100, lambda.min.ratio=0.0001)
best.lambda <- cv.out$lambda.min
best.lambda

#make predictions
x_test <- model.matrix(medv~., test)[,2:12]
y_test <-test$medv
y_predicted <- predict(lasso.mod, s = best.lambda, newx = x_test)
y_predicted

#Total sum of squares
sst <- sum((mean(y_test) - y_test)^2)
#Sum of squares error
sse <- sum((y_predicted - y_test)^2)
#R squared
rsq <- 1 - sse / sst
rsq


########################  Part-B  ######################### 

#performing lasso using biglasso
B_data <-data.matrix(Boston[2:12])
B_data.bm <- as.big.matrix(B_data)
B_data.bm 
is.big.matrix(B_data.bm)
lfit <- biglasso(B_data.bm, Boston$medv)
plot(lfit)
cvfit <- cv.biglasso(B_data.bm, Boston$medv,seed = 100, nfolds = 10)
plot(cvfit)
summary(cvfit)
