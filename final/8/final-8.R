##################################################
# libraries
##################################################
library(fpc)

##################################################
# K-Means Clustering
##################################################
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(url, sep = ",", header = FALSE)
str(iris)

iris.z <- as.data.frame(lapply(iris[-5], scale))
iris.z$V5 <- iris$V5
str(iris.z)

iris.kmeans <- kmeans(iris.z[-5], 3)
iris.kmeans$size
iris.kmeans$centers

iris.z$cluster <- iris.kmeans$cluster
iris.z[, c("V5", "cluster")]

table(iris.z$V5, iris.z$cluster)

aggregate(data = iris.z, iris.z$V1 ~ cluster, mean)
aggregate(data = iris.z, iris.z$V2 ~ cluster, mean)
aggregate(data = iris.z, iris.z$V3 ~ cluster, mean)
aggregate(data = iris.z, iris.z$V4 ~ cluster, mean)

plotcluster(iris.z[-5], iris.z$cluster)

##################################################
# Hierarchical Clustering 
##################################################
iris.matrix <- dist(as.matrix(iris.z[-5]))

iris.hc <- hclust(iris.matrix)
plot(iris.hc) 

iris.hc.cut <- cutree(iris.hc, 3)

table(iris.hc.cut, iris$V5)

iris.hc.2 <- hclust(iris.matrix, method = 'average')
plot(iris.hc.2)

iris.hc.cut.2 <- cutree(iris.hc.2, 3)
table(iris.hc.cut.2, iris$V5)