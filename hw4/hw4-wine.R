# Data Collection and Preparation
wine <- read.csv("whitewines.csv")
str(wine)

# check distribution of target and summary statistics for entire set
hist(wine$quality)
summary(wine)

# create train and test data sets
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# Train Model
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# get basic and more detailed information about the tree
m.rpart
summary(m.rpart)

# create visualization of the regression tree model and add improvements
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Evaluating Model Performance

# generate predictions on test data
p.rpart <- predict(m.rpart, wine_test)

# summary stats for predicted vs. actual
summary(p.rpart)
summary(wine_test$quality)

# correlation value for predicted quality vs. actual
cor(p.rpart, wine_test$quality)

# custom function to calculate mean absolute error for actual vs. predicted results
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

MAE(p.rpart, wine_test$quality)

# find mean of target feature in train data set and calculate MAE if we assigned
# the quality train data mean for every record
mean(wine_train$quality)
MAE(5.87, wine_test$quality)

# Improving Model Performance
# create model tree using MP5
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p

# model summary
summary(m.m5p)

# model predictions and summary
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)

# predicted vs. actual correlation and MAE
cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)