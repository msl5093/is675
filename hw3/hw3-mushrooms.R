# Data Collection and Preparation
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

mushrooms$veil_type <- NULL

table(mushrooms$type)

# Train Model
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

# Evaluate Model Performance
summary(mushroom_1R)

# Improving Performance
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)