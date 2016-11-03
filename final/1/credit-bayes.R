##################################################
# libraries
##################################################
library(e1071)
library(gmodels)

##################################################
# data collection and preparation
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


##################################################
# train model
##################################################
classifier <- naiveBayes(credit_train[-17], credit_train_labels)
classifier
class_test_pred <- predict(classifier, credit_test[-17])


##################################################
# evaluate model performance
##################################################
CrossTable(class_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))


##################################################
# imporving model performance
##################################################
credit <- read.csv("credit.csv")

# convert a few features to factors for easier bayesian classification. these have few possible values
# so factor makes sense
credit$percent_of_income <- as.factor(credit$percent_of_income)
credit$years_at_residence <- as.factor(credit$years_at_residence)
credit$existing_loans_count <- as.factor(credit$existing_loans_count)
credit$dependents <- as.factor(credit$dependents)

set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]

classifier <- naiveBayes(credit_train[-17], credit_train_labels)
class_test_pred <- predict(classifier, credit_test[-17])
CrossTable(class_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))


# laplace estimators:
cred_classifier <- naiveBayes(credit_train, credit_train_labels, laplace = 2)
cred_test_pred <- predict(cred_classifier, credit_test)
CrossTable(cred_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

cred_classifier <- naiveBayes(credit_train, credit_train_labels, laplace = 4)
cred_test_pred <- predict(cred_classifier, credit_test)
CrossTable(cred_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))