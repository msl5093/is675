########
# SPAM
########

# build classifier
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
sms_raw$type <- factor(sms_raw$type)

library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) 

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)

sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)

# obtain raw probabilty numbers
sms_test_prob <- predict(sms_classifier, sms_test, type = "raw")
head(sms_test_prob)

# combine the results into a data frame
sms_results <- data.frame(actual_type = sms_test_labels,
                          predict_type = sms_test_pred,
                          prob_spam = round(sms_test_prob[ , 2], 5),
                          prob_ham = round(sms_test_prob[ , 1], 5))

# low confidence probabilty estimates
head(subset(sms_results, prob_spam > 0.40 & prob_spam < 0.60))

# find where model was wrong
head(subset(sms_results, actual_type != predict_type))

# examine cross tab table of results
table(sms_results$actual_type, sms_results$predict_type)

# same cross tab table but get more descriptive gmodels table
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# detailed confusion matrix using caret
library(caret)
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# kappa via vcd package
library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

# via irr
library(irr)
kappa2(sms_results[1:2])

# sensitivity/specificity
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# precision and recall
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# 
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#########
# credit
#########
library(caret)
credit <- read.csv("credit.csv")

# holdout - random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# stratified holdout using caret
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

library(C50)
library(irr)

credit <- read.csv("credit.csv")

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))