# Data Collection and Preparation
# read in the data and view
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

# convert type feature to a factor
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# read the text feature in as a corpus using the tm package and inspect the resulting object
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

# use the tm_map function to convert words to lowercase
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# use tm_map to remove numbers, stops words, and punctuation from the corpuse
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# word stemming example to see it in action
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))

# use tm_map to stem words in the corpus and eliminate whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) 

lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# create Document Term Matrix from the cleansed corpus
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# create train and test data sets using the DTM
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# create train and test labels to get target features using the original raw data
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels  <- sms_raw[4170:5559, ]$type

# validate ham/spam ratio is reasonable
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# create subsets of orginal raw data for wordcloud exploration
spam <- subset(sms_raw, type == "spam")
ham  <- subset(sms_raw, type == "ham")

# create word clouds, try a few different variations of max words and minimum frequency
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

wordcloud(spam$text, min.freq = 25, scale = c(3, 0.5))
wordcloud(ham$text, min.freq = 25, scale = c(3, 0.5))

# remove terms that do not appear a minimum number of times in the train DTM
sms_dtm_freq_train <- removeSparseTerms(sms_dtm_train, 0.999)
sms_dtm_freq_train

# find words that appear at least five times in the train DTM and save those to a character vector
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

# create train and test DTMs that contain only terms that appear the minimum number of times specified in our frequent words vector
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# custom function to assign counts in place of word frequency values
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply our custom function and create refined test and train data sets
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# Train Model

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# Evaluate Model Performance

sms_test_pred <- predict(sms_classifier, sms_test)

# generate cross table to compare test results to actuals
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

# Improving Performance
# Alternate Solutions
# laplace estimators

# laplace = 1
sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))

# laplace = 2
sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 2)
sms_test_pred <- predict(sms_classifier, sms_test)

# laplace = 4
sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 4)
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual'))