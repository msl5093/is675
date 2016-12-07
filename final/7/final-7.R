##################################################
# libraries
##################################################
library(pscl)

##################################################
# K-Means
##################################################
# data collection
h113 <- readKH("ftp://k7moa.com/dtaord/hou113kh.ord")
s113 <- readKH("ftp://k7moa.com/dtaord/sen113kh.ord")

legis.house_df <- h113$legis.data
votes.house_df <- as.data.frame(h113$votes)
h113_df <- cbind(legis.house_df, votes.house_df)
str(h113_df)

legis.senate_df <- s113$legis.data
votes.senate_df <- as.data.frame(s113$votes)
s113_df <- cbind(legis.senate_df, votes.senate_df)
str(s113_df)



##################################################
# Naive Bayes
##################################################