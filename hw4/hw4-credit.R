# Data Collection and Preparation
credit <- read.csv("credit.csv")
str(credit)

set.seed(123)
train_sample <- sample(1000, 900)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

library(rpart)
credit_tree <- rpart(default ~ ., data = credit_train)

credit_tree
summary(credit_tree)

library(rpart.plot)
rpart.plot(credit_tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

credit_pred <- predict(credit_tree, credit_test, type="class")

CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))