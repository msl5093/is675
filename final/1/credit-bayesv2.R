# Data Collection and Preparation
library(e1071)
library(gmodels)
library(caret)

credit <- read.csv("credit.csv")
str(credit)

set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]

## Train Model
classifier <- naiveBayes(credit_train[-17], credit_train_labels)
classifier

class_test_pred <- predict(classifier, credit_test)

## Evaluating Model Performance
CrossTable(class_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

credit_test_prob <- predict(classifier, credit_test, type = "raw")
head(credit_test_prob)

credit_results <- data.frame(actual_type = credit_test_labels,
  predict_type = class_test_pred,
  prob_no = round(credit_test_prob[ , 1], 5),
  prob_yes = round(credit_test_prob[ , 2], 5))
head(credit_results)

subset(credit_results, actual_type != predict_type)

confusionMatrix(credit_results$predict_type, credit_results$actual_type, positive = "yes")

## Improving Model Performance

### Laplace Estimators
# 8
laplace_8 <- naiveBayes(credit_train[-17], credit_train_labels, laplace = 8)
laplace_8_pred <- predict(laplace_8, credit_test)
confusionMatrix(laplace_8_pred, credit_test_labels, positive = "yes")

# 12
laplace_12 <- naiveBayes(credit_train[-17], credit_train_labels, laplace = 12)
laplace_12_pred <- predict(laplace_12, credit_test)
confusionMatrix(laplace_12_pred, credit_test_labels, positive = "yes")

### Cross Validation
ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.fL = 1, .usekernel = FALSE, .adjust = 1)
set.seed(123)
model = train(credit_train[-17], credit_train_labels, method = 'nb',trControl = ctrl, tuneGrid = grid)

p <- predict(model, credit_test)
confusionMatrix(p, credit_test_labels, positive = "yes")