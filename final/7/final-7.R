##################################################
# libraries
##################################################
library(e1071)
library(gmodels)
library(caret)
library(pscl)
library(fpc)

##################################################
# Data Collection
##################################################
h113 <- readKH("ftp://k7moa.com/dtaord/hou113kh.ord")
s113 <- readKH("ftp://k7moa.com/dtaord/sen113kh.ord")
str(h113)
str(s113)

legis.house_df <- h113$legis.data
votes.house_df <- as.data.frame(h113$votes)
h113_df <- cbind(legis.house_df, votes.house_df)
str(h113_df)

legis.senate_df <- s113$legis.data
votes.senate_df <- as.data.frame(s113$votes)
s113_df <- cbind(legis.senate_df, votes.senate_df)
str(s113_df)

##################################################
# K-Means
##################################################
# House Data
# drop factor features for party and state, they have a numeric pointer anyway
h113_df.noFactors <- h113_df[-1]
h113_df.noFactors <- h113_df.noFactors[-4]
str(h113_df.noFactors)

h113_df.noFactors <- as.data.frame(lapply(h113_df.noFactors, scale))
str(h113_df.noFactors)

house.clusters <- kmeans(h113_df.noFactors, 5)
house.clusters$size

h113_df$cluster <- house.clusters$cluster
h113_df[, c("state", "party", "cluster")]

table(h113_df$party, h113_df$cluster)

table(h113_df$state, h113_df$cluster)

aggregate(data = h113_df, h113_df$`Vote 1` ~ cluster, mean)
aggregate(data = h113_df, h113_df$`Vote 100` ~ cluster, mean)
aggregate(data = h113_df, h113_df$`Vote 500` ~ cluster, mean)
aggregate(data = h113_df, h113_df$`Vote 750` ~ cluster, mean)
aggregate(data = h113_df, h113_df$`Vote 1100` ~ cluster, mean)


# Senate Data
# drop factor features for party and state, they have a numeric pointer anyway
s113_df.noFactors <- s113_df[-1]
s113_df.noFactors <- s113_df.noFactors[-4]

# also drop cd feature as it is not relevant for senators
s113_df.noFactors <- s113_df.noFactors[-2]

s113_df.noFactors <- as.data.frame(lapply(s113_df.noFactors, scale))
str(s113_df.noFactors)

senate.clusters <- kmeans(s113_df.noFactors, 5)
senate.clusters$size
s113_df$cluster <- senate.clusters$cluster

s113_df[, c("state", "party", "cluster")]
table(s113_df$party, s113_df$cluster)

aggregate(data = s113_df, s113_df$`Vote 1` ~ cluster, mean)
aggregate(data = s113_df, s113_df$`Vote 100` ~ cluster, mean)
aggregate(data = s113_df, s113_df$`Vote 200` ~ cluster, mean)
aggregate(data = s113_df, s113_df$`Vote 300` ~ cluster, mean)




##################################################
# Naive Bayes
##################################################
# House Data
set.seed(123)
train_sample <- sample(445, 356)
train <- h113_df[train_sample,]
test <- h113_df[-train_sample,]

prop.table(table(train$party))
prop.table(table(test$party))
train_labels <- train[,"party"]
test_labels <- test[,"party"]

house.classifier <- naiveBayes(train[-5], train_labels)

house.classifier.prediction <- predict(house.classifier, test)
confusionMatrix(house.classifier.prediction, test$party)

# Senate Data
summary(s113_df$party)

set.seed(123)
train_sample <- sample(106, 85)
train <- s113_df[train_sample,]
test <- s113_df[-train_sample,]

prop.table(table(train$party))
prop.table(table(test$party))
train_labels <- train[,"party"]
test_labels <- test[,"party"]

senate.classifier <- naiveBayes(train[-5], train_labels)

senate.classifier.prediction <- predict(senate.classifier, test)
confusionMatrix(senate.classifier.prediction, test$party)