library(readr)
library(nnet)
library(caret)
library(rpart)
library(rpart.plot)
library(car)
# i -----------------------------------------------------------------------
# K means clustering
data <- data.frame(read_csv("dataClustering.csv"))
colnames(data) <- c("var1", "var2", "var3", "var4", "var5", "var6", "var7", "var8")
summary(data)

#Elbow plot
data_scale <- data.frame(scale(data))
set.seed(123)
kmax <- 15
wss <- sapply(1:kmax, function(k) {kmeans(data_scale, k, nstart = 50, iter.max = 15)$tot.withinss} )
plot(1:kmax, wss, type="b",pch = 19, xlab = "Number of clusters K", ylab="Total within-clusters sum of squares")
# Optimal K is 4. 
wss

kmeans4 <- kmeans(data_scale, 4, nstart = 50, iter.max = 15)
data$cluster <- kmeans4$cluster
colnames(data)[9] <- c("cluster")
data$cluster <- as.factor(data$cluster)

# ii ----------------------------------------------------------------------
multinom <- multinom(cluster ~ ., data=data)
summary(multinom)
multipred <- predict(multinom, data, "class")
confusionMatrix(multipred, as.factor(data$cluster))
# Potential problem for multicollinearity 
cor(data[,1:8])

multinom1 <- multinom(cluster ~ var1 + var2 , data=data)
summary(multinom1)
multipred1 <- predict(multinom1, data, "class")
confusionMatrix(multipred1, as.factor(data$cluster))

# Comparing with decision trees
decistree <- rpart(cluster ~ ., data = data, method = "class")
rpart.plot(decistree, yesno = 2, type = 3, extra = 101)
dtpred <- predict(decistree, data, type = "class")
confusionMatrix(dtpred, data$cluster)
