##################################################
# libraries
##################################################
library(car)
library(caret)
library(neuralnet)

##################################################
# Linear Regression
##################################################
cars <- read.csv("ToyotaCorolla.csv")
str(cars)

scatterplot(Price ~ Age, data = cars, xlab="Age", ylab="Price", main="Price Scatterplot", labels=row.names(cars))
scatterplot(Price ~ KM, data = cars, xlab="KM", ylab="Price", main="Price Scatterplot", labels=row.names(cars))

set.seed(123)
train_sample <- sample(1436, 1292)
train <- cars[train_sample, ]
test  <- cars[-train_sample, ]

# linear model using caret
ctrl <- trainControl(method = "cv", number = 10)
model.cars <- train(Price ~., data = train, method = "lm", trControl = ctrl)
summary(model.cars)

model.prediction <- predict(model.cars, test)
cor(model.prediction, test$Price)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

MAE(test$Price, model.prediction)

##################################################
# Artificial Neural Network
##################################################
str(cars)
cars$FuelType <- as.numeric(cars$FuelType)
str(cars)

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

cars.norm <- as.data.frame(lapply(cars, normalize))
str(cars.norm)

set.seed(123)
train_sample <- sample(1436, 1292)
train.norm <- cars.norm[train_sample, ]
test.norm  <- cars.norm[-train_sample, ]

set.seed(12345)
cars.ann <- neuralnet(formula = Price ~ Age + KM + FuelType + HP + MetColor + Automatic + CC + Doors + Weight, 
                      data = train.norm)

plot(cars.ann, rep="best")

cars.ann.results <- compute(cars.ann, test.norm[2:10])
cars.ann.prediction <- cars.ann.results$net.result
cor(cars.ann.prediction, test.norm$Price)

MAE(test.norm$Price, cars.ann.prediction)