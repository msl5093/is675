##################################################
# libraries
##################################################
library(neuralnet)
library(e1071)
library(gmodels)
library(caret)

url = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(url, sep = ",", header = FALSE)
str(iris)

set.seed(123)
train_sample <- sample(150, 105)
train <- iris[train_sample,]
test <- iris[-train_sample,]

prop.table(table(train$V5))
prop.table(table(test$V5))

train_labels <- train[,"V5"]
test_labels <- test[,"V5"]

##################################################
# Naive Bayes
##################################################
classifier <- naiveBayes(train[-5], train_labels)
classifier

test.pred <- predict(classifier, test)

CrossTable(test.pred, test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))
confusionMatrix(test.pred, test_labels)

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.fL = 1, .usekernel = FALSE, .adjust = 1)
set.seed(123)
model = train(train[-5], train_labels, method = 'nb',trControl = ctrl, tuneGrid = grid)

p <- predict(model, test)
confusionMatrix(p, test_labels)


## ANN
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
iris <- read.table(url, sep = ",", header = FALSE)
str(iris)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

summary(iris)
iris.norm <- as.data.frame(lapply(iris[1:4], normalize))
iris.norm$V5 <- iris[,5]
summary(iris.norm)

iris.norm$setosa <- c(iris.norm$V5 == "Iris-setosa")
iris.norm$versicolor <- c(iris.norm$V5 == "Iris-versicolor")
iris.norm$virginica <- c(iris.norm$V5 == "Iris-virginica")
str(iris.norm)

set.seed(123)
train_sample <- sample(150, 112)
train <- iris.norm[train_sample,]
test <- iris.norm[-train_sample,]

train_labels <- train[,"V5"]
test_labels <- test[,"V5"]

iris.Ann <- neuralnet(setosa + versicolor + virginica ~ V1 + V2 + V3 + V4, data = train)
plot(iris.Ann, rep = "best")

iris.prediction <- compute(iris.Ann, test[1:4])
iris.prediction$net.result



CrossTable(result, test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))