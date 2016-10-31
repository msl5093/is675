##################################################
# libraries
##################################################
library(class)
library(gmodels)
library(car)


##################################################
# data collection and preparation
##################################################
credit <- read.csv("credit.csv")


# check for distribution of numeric variables
summary(credit[sapply(credit, is.numeric)]) # months_loan_duration, amount, age


# normalize numeric variables with large range
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}
credit["months_loan_duration"] <- lapply(credit["months_loan_duration"], normalize)
credit["amount"] <- lapply(credit["amount"], normalize)
credit["age"] <- lapply(credit["age"], normalize)


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


# create train and test data sets using random sample
set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]


# create train and test class objects
credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]


# drop default factor from train and test data sets
credit_train <- credit_train[-17]
credit_test <- credit_test[-17]

##################################################
# train model
##################################################
# k = sq rt (900 = total train records)
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 30)
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq = FALSE)


##################################################
# evaluate model performance
##################################################


##################################################
# imporving model performance
##################################################

# different number of neighbors
credit_test_pred2 <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 60)
CrossTable(x = credit_test_labels, y = credit_test_pred2, prop.chisq = FALSE)

credit_test_pred3 <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 40)
CrossTable(x = credit_test_labels, y = credit_test_pred3, prop.chisq = FALSE)

# z-score normalization