##################################################
# libraries
##################################################
library(e1071)


##################################################
# data collection and preparation
##################################################
credit <- read.csv("credit.csv")

set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

credit_train_labels <- credit_train[,"default"]
credit_test_labels <- credit_test[,"default"]

classifier <- naiveBayes(credit_train, credit_train_labels)

class_test_pred <- predict(classifier, credit_test)

CrossTable(class_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))


cred_classifier <- naiveBayes(credit_train, credit_train_labels, laplace = 4)
cred_test_pred <- predict(cred_classifier, credit_test)

CrossTable(cred_test_pred, credit_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))