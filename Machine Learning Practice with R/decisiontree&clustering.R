library(MASS)
library(factoextra)
library(NbClust)
library(partykit)

Shuffle <- sample(1:nrow(ChickWeight))
dt<-ChickWeight
dt$Diet <- factor(dt$Diet, levels = c(1,2,3,4))
#Splitting the data
set.seed(100) 
row.number <- sample(x=1:nrow(dt), size=0.8*nrow(dt))
traindt = dt[row.number,]
testdt = dt[-row.number,]

#Building a classification Decision tree
ct <- ctree(Diet~weight+Time, data = traindt)
plot(ct)

#Predict
predict(ct, testdt, type="prob")

#k-means clustering
dt1 = attitude[,c(2,3)]
plot(dt1, main = "")
set.seed(5)
km1 = kmeans(dt1, 2, nstart = 100)
plot(dt1, col =(km1$cluster +1) , main="K-Means result with 2 clusters")
km1$cluster
dt2 <- dt1
wss <- (nrow(dt2)-1)*sum(apply(dt2,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(dt2,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method")
set.seed(5)
km2 = kmeans(dt1, 10, nstart=100)
plot(dt1, col =(km2$cluster +1) , main="K-Means result with 10 clusters")
km2$cluster


#density based clustering
dt2 <- iris[,-5]
kNNdistplot(dt2, k=4)
db <- dbscan(dt2, eps=0.45, minPts = 4)
db
plot(dt2, col=db$cluster+1)
