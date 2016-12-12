##################################################
# libraries
##################################################
library(nnet)
library(e1071)
library(gmodels)
library(caret)

##################################################
# Naive Bayes
##################################################
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

classifier <- naiveBayes(train[-5], train_labels)

test.pred <- predict(classifier, test)
confusionMatrix(test.pred, test_labels)

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.fL = 1, .usekernel = FALSE, .adjust = 1)
set.seed(123)
model = train(train[-5], train_labels, method = 'nb',trControl = ctrl, tuneGrid = grid)
model

p <- predict(model, test)
confusionMatrix(p, test_labels)


##################################################
# Artifical Neural Network
##################################################
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

summary(iris)
iris.norm <- as.data.frame(lapply(iris[1:4], normalize))
iris.norm$V5 <- iris[,5]
summary(iris.norm)

set.seed(123)
train_sample <- sample(150, 112)
train <- iris.norm[train_sample,]
test <- iris.norm[-train_sample,]
train_labels <- train[,"V5"]
test_labels <- test[,"V5"]

iris.ann <- nnet(V5 ~ ., data = train, size = 1)

iris.prediction <- predict(iris.ann, test, type = "class")
confusionMatrix(iris.prediction, test_labels)