# Data Collection and Preparation
concrete <- read.csv("concrete.csv")
str(concrete)
summary(concrete)

# normalize numeric features to shrink range of values
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame and store results in new object
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# Train Model
library(neuralnet)

# ANN with only a single hidden neuron
set.seed(12345)
concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic
                            + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model, rep="best")

# Evaluating Model Performance
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result

# correlation between predicted vs. actual strength values
cor(predicted_strength, concrete_test$strength)

# Improving Model Performance
set.seed(12345)
# create ANN model with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

# additional improvements -> add more hidden nodes (10)
set.seed(12345)
concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 10)
plot(concrete_model3)
model_results3 <- compute(concrete_model3, concrete_test[1:8])
predicted_strength3 <- model_results3$net.result
cor(predicted_strength3, concrete_test$strength)