##################################################
# libraries
##################################################
library(arules)
library(RWeka)

##################################################
# Association Rules - no type variable
##################################################
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)
mushrooms$veil_type <- NULL

# omit type variable
mushrooms.data <- as(mushrooms[-1], "transactions")
mushrooms.data

mushroom.rules <- apriori(mushrooms.data, parameter = list(support = 0.1, confidence = 0.75, minlen = 2))
mushroom.rules

summary(mushroom.rules)
inspect(mushroom.rules[1:25])

inspect(sort(mushroom.rules, by = "lift")[1:20])

mushroom.odor.foul <- subset(mushroom.rules, lhs %in% "odor=foul")
inspect(sort(mushroom.odor.foul, by = "lift")[1:25])

mushroom.odor.none <- subset(mushroom.rules, lhs %in% "odor=none")
inspect(sort(mushroom.odor.none, by = "lift")[1:25])

mushroom.rules_df <- as(mushroom.rules, "data.frame")
str(mushroom.rules_df)


##################################################
# One Rule Learner
##################################################
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R
summary(mushroom_1R)


##################################################
# Association Rules - with type variable
##################################################
mushrooms.data.type <- as(mushrooms, "transactions")

mushroom.rules_2 <- apriori(mushrooms.data.type, parameter = list(support = 0.1, confidence = 0.75, minlen = 2), appearance = list(rhs=c("type=edible", "type=poisonous")))
mushroom.rules_2
summary(mushroom.rules_2)

mushroom.type.edible <- subset(mushroom.rules_2, rhs %in% "type=edible")
inspect(sort(mushroom.type.edible, by = "lift")[1:25])

mushroom.type.poisonous <- subset(mushroom.rules_2, rhs %in% "type=poisonous")
inspect(sort(mushroom.type.poisonous, by = "lift")[1:25])

mushroom.type.edible.odor <- subset(mushroom.type.edible, lhs %in% "odor=none")
inspect(sort(mushroom.type.edible.odor, by = "lift")[1:25])

mushroom.type.poisonous.odor <- subset(mushroom.type.poisonous, lhs %in% "odor=foul")
inspect(sort(mushroom.type.poisonous.odor, by = "lift")[1:25])

mushrooms.ringtype.large <- subset(mushroom.type.poisonous, lhs %in% "ring_type=large")
inspect(sort(mushrooms.ringtype.large, by = "lift")[1:25])

mushroom_1R