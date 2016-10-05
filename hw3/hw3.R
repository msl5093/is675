# load data and examine characteristics
credit <- read.csv("credit.csv")
str(credit)

# examine checking and savings balance vectors
table(credit$checking_balance)
table(credit$savings_balance)

# examine two features related to the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# examine the class feature we are going to predict
table(credit$default)

# since the data is sorted by x, set a random seed to reorder the data
# to build train and test data sets
set.seed(123)

# initialize a train_sample data set with 900 values raning from 1-1000
train_sample <- sample(1000, 900)
str(train_sample)

# create train and test data sets using our sample values to get rows from the
# credit data frame for each set
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the ratio of class variable: default to ensure they are consistent between train and test
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# load C50 library for modeling decision tree and create a basic decision tree model
# pass the entire credit_train data frame to the C5.0 function less the class variable
# as well as the class variable for which we are predicting
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

# display details about the tree and summary information
credit_model
summary(credit_model)

# create vector for predictions using our credit_model and test data set
credit_pred <- predict(credit_model, credit_test)

# build corss tabulation to compare model results to actuals
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))