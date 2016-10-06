# Data Collection and Preparation
credit <- read.csv("credit.csv")
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)
set.seed(123)

train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Train Model
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

credit_model
summary(credit_model)

credit_pred <- predict(credit_model, credit_test)

# Evaluate Model Performance
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# Improving Performance
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

# ctree ()
# Statistics-based approach that uses non-parametric tests as splitting criteria, 
# corrected for multiple testing to avoid overfitting. 
# This approach results in unbiased predictor selection and does not require pruning.
library(party)

ctree_obj <- ctree(default ~ ., data = credit)
ctree_obj
plot(ctree_obj)

set.seed(123)
train_sample <- sample(1000, 900)
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]
ctree_model <- ctree(default ~ ., data = credit_train)
plot(ctree_model)
ctree_predict <- predict(ctree_model, credit_test)
CrossTable(credit_test$default, ctree_predict, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))