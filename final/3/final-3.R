##################################################
# libraries
##################################################
library(psych)
library(rpart)
library(rpart.plot)
library(RWeka)

prostate <- read.csv("prostate.csv")
str(prostate)

# exploratory analysis
summary(prostate$lpsa)
hist(prostate$lpsa)

summary(prostate$lcavol)
hist(prostate$lcavol)

pairs.panels(prostate[c("lcavol", "age", "lbph", "lcp", "gleason", "lpsa")])

# correlation between lpsa and lcavol
cor(prostate[c("lpsa", "lcavol")])

set.seed(123)
train_sample <- sample(97, 68)
train <- prostate[train_sample, ]
test  <- prostate[-train_sample, ]

##################################################
# Regression Trees
##################################################
## lpsa
model.lpsa.tree <- rpart(lpsa ~ ., data = train)
model.lpsa.tree
rpart.plot(model.lpsa.tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

model.prediction <- predict(model.lpsa.tree, test)
summary(model.prediction)
summary(test$lpsa)

cor(model.prediction, test$lpsa)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

MAE(test$lpsa, model.prediction)

###
m.m5p <- M5P(lpsa ~ ., data = train)
summary(m.m5p)

p.m5p <- predict(m.m5p, test)
summary(p.m5p)

cor(p.m5p, test$lpsa)
MAE(test$lpsa, p.m5p)


## lcavol
model.lcavol.tree <- rpart(lcavol ~ ., data = train, method = "anova")
model.lcavol.tree
rpart.plot(model.lcavol.tree, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

model.prediction <- predict(model.lcavol.tree, test)
summary(model.prediction)
summary(test$lcavol)

cor(model.prediction, test$lcavol)

MAE(test$lcavol, model.prediction)

###
m.m5p <- M5P(lcavol ~ ., data = train)
summary(m.m5p)

p.m5p <- predict(m.m5p, test)
summary(p.m5p)

cor(p.m5p, test$lcavol)
MAE(test$lcavol, p.m5p)

##################################################
# Linear Regression
##################################################
## lpsa
# basic linear model
model.lpsa.lm <- lm(lpsa ~ ., data = train)
model.lpsa.lm
summary(model.lpsa.lm)

model.prediction <- predict(model.lpsa.lm, test)
cor(model.prediction, test$lpsa)
MAE(test$lpsa, model.prediction)


## lcavol
model.lcavol.lm <- lm(lcavol ~ ., data = train)
model.lcavol.lm
summary(model.lcavol.lm)

model.prediction <- predict(model.lcavol.lm, test)
cor(model.prediction, test$lcavol)
MAE(test$lcavol, model.prediction)