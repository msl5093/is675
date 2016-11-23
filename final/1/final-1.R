##################################################
# libraries
##################################################
library(e1071)
library(class)
library(gmodels)
library(car)
library(caret)

##################################################
# Naive Bayes
##################################################
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

classifier <- naiveBayes(credit_train[-17], credit_train_labels)
classifier

class_test_pred <- predict(classifier, credit_test)

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

# 8
laplace_8 <- naiveBayes(credit_train[-17], credit_train_labels, laplace = 8)
laplace_8_pred <- predict(laplace_8, credit_test)
confusionMatrix(laplace_8_pred, credit_test_labels, positive = "yes")

# 12
laplace_12 <- naiveBayes(credit_train[-17], credit_train_labels, laplace = 12)
laplace_12_pred <- predict(laplace_12, credit_test)
confusionMatrix(laplace_12_pred, credit_test_labels, positive = "yes")

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.fL = 1, .usekernel = FALSE, .adjust = 1)
set.seed(123)
model = train(credit_train[-17], credit_train_labels, method = 'nb',trControl = ctrl, tuneGrid = grid)

p <- predict(model, credit_test)
confusionMatrix(p, credit_test_labels, positive = "yes")

##################################################
# kNN
##################################################
credit <- read.csv("credit.csv")
str(credit)

scatterplot(credit$amount ~ credit$months_loan_duration | credit$default) # defaults appear randomly scattered along both amount and months of the loan
scatterplot(credit$amount ~ credit$age | credit$default) # also random scatter

# how do loan amounts change over the length of the loan
boxplot(amount ~ months_loan_duration, data = credit, main="Credit Data", xlab = "months_loan_duration", ylab = "amount")

# check for distribution of numeric variables
summary(credit[sapply(credit, is.numeric)]) # months_loan_duration, amount, age

# normalize numeric variables with large range
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
credit["months_loan_duration"] <- lapply(credit["months_loan_duration"], normalize)
credit["amount"] <- lapply(credit["amount"], normalize)
credit["age"] <- lapply(credit["age"], normalize)
str(credit)

# convert factors to numeric
summary(credit[sapply(credit, is.factor)])
credit$checking_balance <- as.numeric(credit$checking_balance)
credit$credit_history <- as.numeric(credit$credit_history)
credit$purpose <- as.numeric(credit$purpose)
credit$savings_balance <- as.numeric(credit$savings_balance)
credit$employment_duration <- as.numeric(credit$employment_duration)
credit$other_credit <- as.numeric(credit$other_credit)
credit$housing <- as.numeric(credit$housing)
credit$job <- as.numeric(credit$job)
credit$phone <- as.numeric(credit$phone)
str(credit)

# create train and test data sets using random sample
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# create train and test class objects
credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]

# drop default factor from train and test data sets
credit_train <- credit_train[-17]
credit_test <- credit_test[-17]

# k = sq rt (900 = total train records)
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 30)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq = FALSE)
confusionMatrix(credit_test_labels, credit_test_pred, positive = "yes")

# different k values
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 50)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq = FALSE)
confusionMatrix(credit_test_labels, credit_test_pred, positive = "yes")

credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 20)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq = FALSE)
confusionMatrix(credit_test_labels, credit_test_pred, positive = "yes")

# read data back in as caret will perform some normalization itself
credit <- read.csv("credit.csv")

# convert factors to numeric
credit$checking_balance <- as.numeric(credit$checking_balance)
credit$credit_history <- as.numeric(credit$credit_history)
credit$purpose <- as.numeric(credit$purpose)
credit$savings_balance <- as.numeric(credit$savings_balance)
credit$employment_duration <- as.numeric(credit$employment_duration)
credit$other_credit <- as.numeric(credit$other_credit)
credit$housing <- as.numeric(credit$housing)
credit$job <- as.numeric(credit$job)
credit$phone <- as.numeric(credit$phone)

# create train and test data sets using random sample
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

# create train and test class objects
credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]

set.seed(400)
ctrl <- trainControl(method = "cv", number = 10)
knnFit <- train(default ~ ., data = credit_train, method = "knn", metric = "Kappa", trControl = ctrl, preProcess = c("center", "scale"))

knnFit

p <- predict(knnFit, credit_test[-17])
CrossTable(x = credit_test_labels, y = p, prop.chisq = FALSE)
confusionMatrix(credit_test_labels, p, positive = "yes")