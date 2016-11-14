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
sms_dtm_freq_train

findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_train_labels)