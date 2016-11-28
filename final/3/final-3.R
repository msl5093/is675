##################################################
# libraries
##################################################
library(psych)
library(rpart)
library(rpart.plot)

##################################################
# Linear Regression
##################################################
prostate <- read.csv("prostate.csv")
str(prostate)

set.seed(123)
train_sample <- sample(97, 83)
train <- prostate[train_sample, ]
test  <- prostate[-train_sample, ]

## lpsa
# exploratory analysis for lpsa
summary(prostate$lpsa)
hist(prostate$lpsa)
pairs.panels(prostate[c("lcavol", "age", "lbph", "lcp", "gleason", "lpsa")])

# correlation between lpsa and lcavol
cor(prostate[c("lpsa", "lcavol")])

# basic linear model
model.lpsa_lm <- lm(lpsa ~ ., data = train)
model.lpsa_lm
summary(model.lpsa_lm)


## lcavol
model.lcavol_lm <- lm(lcavol ~ ., data = train)
model.lcavol_lm
summary(model.lcavol_lm)

cor(prostate[c("lcavol", "lcp")])

##################################################
# Regression Trees
##################################################

## lpsa
model.lpsa_tree <- rpart(lpsa ~ ., data = train, method = "anova")
model.lpsa_tree
rpart.plot(model.lpsa_tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)


## lcavol
model.lcavol_tree <- rpart(lcavol ~ ., data = train, method = "anova")
model.lcavol_tree
rpart.plot(model.lcavol_tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
