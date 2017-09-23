# clear environment variables
rm(list = ls())

library(mclust)
library(cluster)

denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_dataset.csv")
denied_claims[is.na(denied_claims)] <- 0
head(denied_claims)
# denied_claims <- denied_claims[, c(1, 3)]
denied_claims <- denied_claims[, c(3, 4)]
head(denied_claims)
# denied_claims$RSN1 <- as.factor(denied_claims$RSN1)
# 
mod1 = Mclust(denied_claims, 7)
# 
summary(mod1, parameters = TRUE)
plot( mod1, what = "classification", dimens = c(1,2))
# 
# library(ggplot2)
# ggplot(denied_claims, aes(TotalChg, RSN1, color = RSN1)) + geom_point()
# 
# # use ggplot
# library(ggplot2)
# denied_claims[is.na(denied_claims)] <- 0
# 
# Cluster <- kmeans(denied_claims[, 1,2], 6, nstart = 1)



# C:/Users/anunchim/Documents/denied_claims.csv
denied_claims <- read.csv("C:/Users/anunchim/Documents/denied_claims.csv")
head(denied_claims)
mod1 = Mclust(denied_claims, 8)

summary(mod1, parameters = TRUE)
plot( mod1, what = "classification", dimens = c(1,2))

library(ggplot2)
ggplot(denied_claims, aes(TotalChg, RSN1, color = RSN1)) + geom_point()
