# clear environment variables
rm(list = ls())

# read accepted claims dataset
accepted.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_synthetic_dataset.csv", sep=",", na = "0")

# read denied claims dataset
denied.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_synthetic_dataset.csv", sep=",", na = "0")

# Replace all negative values by zero
accepted.data$Days <- ifelse(accepted.data$Days < 0, 0, accepted.data$Days)
denied.data$Days <- ifelse(denied.data$Days < 0, 0, denied.data$Days)

accepted.data[is.na(accepted.data)] <- 0
denied.data[is.na(denied.data)] <- 0

# samples from accepted and denied claims datasets
m <- accepted.data[sample(nrow(accepted.data), 10000, rep = FALSE), ]
n <- denied.data[sample(nrow(denied.data), 3000, rep = FALSE), ]

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
final.dataset$Marker <- NULL

# Likely29
final.dataset$Likely29 <- ifelse(final.dataset$Days > 30, "Yes", "No")

# Likely45
final.dataset$Likely45 <- ifelse(final.dataset$TotalChg > 46000, "Yes", "No")

# Expire time limit 
final.dataset$ExpireTimeLimit <- ifelse(final.dataset$Days > 45, "Yes", "No")





