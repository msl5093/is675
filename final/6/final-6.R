##################################################
# libraries
##################################################
library(psych)
library(caret)
library(rpart)
library(rpart.plot)

##################################################
# Linear Regression
##################################################
cars <- read.csv("ToyotaCorolla.csv")
str(cars)

cars$MetColor <- factor(cars$MetColor, levels = c(0,1), labels = c("no", "yes"))
cars$Automatic <- factor(cars$Automatic, levels = c(0,1), labels = c("no", "yes"))
str(cars)

# linear correlations
pairs.panels(cars[c("Price", "Age", "KM", "Doors", "HP")])

set.seed(123)
train_sample <- sample(1436, 1148)
train <- cars[train_sample, ]
test  <- cars[-train_sample, ]

ctrl <- trainControl(method = "cv", number = 10)
model.cars <- train(Price ~., data = train, method = "lm", trControl = ctrl)
summary(model.cars)

model.prediction <- predict(model.cars, test)

plot(model.prediction, test$Price, xlab = "Predicted Price", ylab = "Actual Price")

cor(model.prediction, test$Price)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

MAE(test$Price, model.prediction)

##################################################
# Regression Tree
##################################################
model.tree.cars <- train(Price ~., data = train, method = "rpart", trControl = ctrl)
model.tree.cars

p.rpart <- predict(model.tree.cars, test)
cor(p.rpart, test$Price)
MAE(p.rpart, test$Price)

# complexity parameter
grid <- expand.grid(.cp=seq(0.005, 0.05, 0.025)) 
model.tree.cars <- train(Price ~., data = train, method = "rpart", trControl = ctrl, tuneGrid = grid)
model.tree.cars

p.rpart <- predict(model.tree.cars, test)
cor(p.rpart, test$Price)
MAE(p.rpart, test$Price)