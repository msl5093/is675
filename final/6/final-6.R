library(neuralnet)

##################################################
# Linear Regression
##################################################
cars <- read.csv("ToyotaCorolla.csv")
str(cars)

set.seed(123)
train_sample <- sample(1436, 1292)
train <- cars[train_sample, ]
test  <- cars[-train_sample, ]

# basic linear model
model.cars <- lm(Price ~ ., data = train)
model.cars
summary(model.cars)

model.prediction <- predict(model.cars, test)
cor(model.prediction, test$Price)

residual.train <- train$Price - predict(model.cars, data = train)
residual.test <- test$Price - model.prediction

sqrt(mean(residual.train^2))
sqrt(mean(residual.test^2))

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


# 5 hidden nodes
set.seed(12345)
cars.ann.hidden5 <- neuralnet(formula = Price ~ Age + KM + FuelType + HP + MetColor + Automatic + CC + Doors + Weight, 
                      data = train.norm, hidden = 5)

plot(cars.ann.hidden5, rep="best")

cars.ann.hidden5.results <- compute(cars.ann.hidden5, test.norm[2:10])
cars.ann.hidden5.prediction <- cars.ann.hidden5.results$net.result
cor(cars.ann.hidden5.prediction, test.norm$Price)


# caret
grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(2, 4, 6))
ctrl <- trainControl(method = "cv", number = 10)
  
cars.model.nnet <- train(Price ~ ., data = train.norm, method = "nnet", tuneGrid = grid, tuneControl = ctrl)

cars.model.nnet.prediction <- predict(cars.model.nnet, test.norm)
cor(cars.model.nnet.prediction, test.norm$Price)