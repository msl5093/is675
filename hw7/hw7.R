# Data Collection and Preparation
teens <- read.csv("snsdata.csv")
str(teens)

table(teens$gender)
table(teens$gender, useNA = "ifany")

summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# NA gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# mean age by cohort
mean(teens$age)
mean(teens$age, na.rm = TRUE)

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# create a vector with the average age for each gradyear, repeated by person
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

# Train Model
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

# Evaluating Model Performance
teen_clusters$size

# look at the cluster centers
teen_clusters$centers

# Improving Model Performance
teens$cluster <- teen_clusters$cluster
teens[1:5, c("cluster", "gender", "age", "friends")]

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)

library(fpc)
plotcluster(teens[5:40], teens$cluster)