# Data Collection and Preparation
letters <- read.csv("letterdata.csv")
str(letters)

letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

# Train Model
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier

# Evaluating Model Performance
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)

agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

# Improving Model Performance
# Gaussian kernel
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

# polynomial kernel
set.seed(12345)
letter_classifier_poly <- ksvm(letter ~ ., data = letters_train, kernel = "polydot")
letter_predictions_poly <- predict(letter_classifier_poly, letters_test)

agreement_poly <- letter_predictions_poly == letters_test$letter
table(agreement_poly)
prop.table(table(agreement_poly))