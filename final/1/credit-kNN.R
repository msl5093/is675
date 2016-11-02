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

##################################################
# train model
##################################################
# k = sq rt (900 = total train records)
credit_test_pred <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 30)


##################################################
# evaluate model performance
##################################################
CrossTable(x = credit_test_labels, y = credit_test_pred, prop.chisq = FALSE)


##################################################
# imporving model performance
##################################################

# different number of neighbors
credit_test_pred2 <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 40)
CrossTable(x = credit_test_labels, y = credit_test_pred2, prop.chisq = FALSE)

credit_test_pred3 <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 20)
CrossTable(x = credit_test_labels, y = credit_test_pred3, prop.chisq = FALSE)

credit_test_pred4 <- knn(train = credit_train, test = credit_test, cl = credit_train_labels, k = 10)
CrossTable(x = credit_test_labels, y = credit_test_pred4, prop.chisq = FALSE)



# z-score normalization
credit_z <- read.csv("credit.csv")
credit_z["months_loan_duration"] <- as.data.frame(scale(credit_z["months_loan_duration"]))
credit_z["amount"] <- as.data.frame(scale(credit_z["amount"]))
credit_z["age"] <- as.data.frame(scale(credit_z["age"]))

summary(credit_z[sapply(credit_z, is.numeric)]) # months_loan_duration, amount, age

credit_z$checking_balance <- as.numeric(credit_z$checking_balance)
credit_z$credit_history <- as.numeric(credit_z$credit_history)
credit_z$purpose <- as.numeric(credit_z$purpose)
credit_z$savings_balance <- as.numeric(credit_z$savings_balance)
credit_z$employment_duration <- as.numeric(credit_z$employment_duration)
credit_z$other_credit <- as.numeric(credit_z$other_credit)
credit_z$housing <- as.numeric(credit_z$housing)
credit_z$job <- as.numeric(credit_z$job)
credit_z$phone <- as.numeric(credit_z$phone)

credit_z_train <- credit_z[train_sample,]
credit_z_test <- credit_z[-train_sample,]

credit_z_train_labels <- credit_z_train[,"default"]
credit_z_test_labels <- credit_z_test[,"default"]

credit_z_train <- credit_z_train[-17]
credit_z_test <- credit_z_test[-17]

credit_z_test_pred <- knn(train = credit_z_train, test = credit_z_test, cl = credit_z_train_labels, k = 30)
CrossTable(x = credit_z_test_labels, y = credit_z_test_pred, prop.chisq = FALSE)

credit_z_test_pred <- knn(train = credit_z_train, test = credit_z_test, cl = credit_z_train_labels, k = 10)
CrossTable(x = credit_z_test_labels, y = credit_z_test_pred, prop.chisq = FALSE)