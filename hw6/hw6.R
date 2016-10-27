## Data Collection and Preparation
library(arules)
groceries <- read.transactions("groceries.csv", sep = ",")
summary(groceries)

inspect(groceries[1:5])

itemFrequency(groceries[, 1:3])

itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:5])
image(sample(groceries, 100))

## Train Model
apriori(groceries)
groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules

## Evaluating Model Performance
summary(groceryrules)
inspect(groceryrules[1:3])

## Improving Model Performance
inspect(sort(groceryrules, by = "lift")[1:5])

berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# find other possible subset pairings
inspect(sort(groceryrules, by = "lift")[1:10])

# other vegetables only:
vegrules <- subset(groceryrules, items %in% "other vegetables")
inspect(sort(vegrules, by = "lift")[1:10])

# whole milk - lhs only
milkrules <- subset(groceryrules, lhs %in% "whole milk")
inspect(sort(milkrules, by = "lift")[1:10])

write(groceryrules, file = "groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)