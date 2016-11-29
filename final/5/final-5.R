library(arules)

# Data Collection and Preparation
mushrooms <- read.transactions("mushrooms.csv", sep = ",")
summary(mushrooms)

inspect(mushrooms[1:3])
itemFrequency(mushrooms[, 1:3])

itemFrequencyPlot(mushrooms, support = 0.1)
itemFrequencyPlot(mushrooms, topN = 30)

image(sample(mushrooms, 100))

mushroom.rules <- apriori(mushrooms, parameter = list(support = 0.1, confidence = 0.75, minlen = 2))
mushroom.rules

summary(mushroom.rules)

inspect(mushroom.rules[1:25])

inspect(sort(mushroom.rules, by = "lift")[1:5])

mushroom.chocrules <- subset(mushroom.rules, lhs %in% "chocolate")
inspect(sort(mushroom.chocrules, by = "lift")[1:25])

mushroom.silkyrules <- subset(mushroom.rules, lhs %in% "silky")
inspect(sort(mushroom.silkyrules, by = "lift")[1:25])

mushroom.notlargerhs <- subset(mushroom.rules, !rhs %in% "large")
inspect(sort(mushroom.notlargerhs, by = "lift")[1:25])

mushroom.rules_df <- as(mushroom.rules, "data.frame")
str(mushroom.rules_df)