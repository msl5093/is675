# Data Collection and Preparation

# read and examine the data
insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the dependent variable and check for distribution of values
summary(insurance$expenses)
hist(insurance$expenses)

# check distribution of categorical variable region
table(insurance$region)

# examine relationships using correlation matrix and visualize those relationships using scatterplot matrix
cor(insurance[c("age", "bmi", "children", "expenses")])
pairs(insurance[c("age", "bmi", "children", "expenses")])

# use psych library for a more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])

# Train Model

# construct linear model using independent features and dependent target
ins_model <- lm(expenses ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model <- lm(expenses ~ ., data = insurance)

# view model/beta coefficients
ins_model

# Evaluating Model Performance

summary(ins_model)

# Improving Model Performance

# add polynomial level (for non-linear correlation) to age feature as new vector in data frame (power of 2)
insurance$age2 <- insurance$age^2

# convert numeric variable into binary indicator with threshold of 30 for BMI
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)
