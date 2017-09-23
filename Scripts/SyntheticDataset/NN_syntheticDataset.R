# clear environment variables
rm(list = ls())

library(neuralnet)
library(ReporteRs)

# Read train dataset
accepted_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_synthetic_dataset.csv")
denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_synthetic_dataset.csv")

accepted_claims[is.na(accepted_claims)] <- 0
denied_claims[is.na(denied_claims)] <- 0

# Replace all negative values by zero
accepted.data$Days <- ifelse(accepted.data$Days < 0, 0, accepted.data$Days)
denied.data$Days <- ifelse(denied.data$Days < 0, 0, denied.data$Days)

## set the seed to make your partition reproductible
# set.seed(123)

# samples from accepted and denied claims datasets
m <- accepted_claims[sample(nrow(accepted_claims), 10000, rep = FALSE), ]
n <- denied_claims[sample(nrow(denied_claims), 3000, rep = FALSE), ]

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
head(final.dataset, 500)
final.dataset$RSN1 <- as.numeric(ifelse(final.dataset$RSN1 == 100, 100, 200))

# Likely29
# final.dataset$Likely29 <- ifelse(final.dataset$Days > 30, "Yes", "No")
final.dataset$Likely29 <- ifelse(final.dataset$Days >= 0 & final.dataset$Days < 1000, 0.2,
                                 ifelse(final.dataset$Days >= 1000 & final.dataset$Days < 2000, 0.4,
                                        ifelse(final.dataset$Days >= 2000 & final.dataset$Days < 3000, 0.6,
                                               ifelse(final.dataset$Days >= 3000 & final.dataset$Days < 4000, 0.7,
                                                      ifelse(final.dataset$Days >= 4000, 0.8,0)))))


# Likely45
# final.dataset$Likely45 <- ifelse(final.dataset$TotalChg > 46000, "Yes", "No")
final.dataset$Likely45 <- ifelse(final.dataset$TotalChg >= 0 & final.dataset$TotalChg < 10000, 0.1,
                                 ifelse(final.dataset$TotalChg >= 10000 & final.dataset$TotalChg < 50000, 0.3,
                                        ifelse(final.dataset$TotalChg >= 50000 & final.dataset$TotalChg < 100000, 0.5,
                                               ifelse(final.dataset$TotalChg >= 100000 & final.dataset$TotalChg < 250000, 0.7,
                                                      ifelse(final.dataset$TotalChg >= 200000, 0.9, 0)))))


# Expire time limit 
final.dataset$ExpireTimeLimit <- ifelse(final.dataset$Days > 45, "Yes", "No")

head(final.dataset, 500)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))
train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

head(train)
head(test)


## build the neural network (NN)
neuralnetwork <- neuralnet(RSN1 ~ TotalChg + Days + TotalPay, train, hidden = 5, lifesign = "minimal", 
                           linear.output = FALSE, threshold = 0.1)

## plot the NN
plot(neuralnetwork, rep = "best")

## test the resulting output
temp_test <- subset(test, select = c("TotalChg", "Days", "TotalPay"))

results <- compute(neuralnetwork, temp_test)

head(temp_test)

results <- data.frame(actual = test$RSN1, prediction = results$net.result)
results[1:15, ]

# We can round to the nearest integer to improve readability:
### check if it is valid to multiply prediction by 100. 
results$prediction <- round(results$prediction)* 100
results[1:15, ]

# confusion matrix for analysis
xtab <- table(Prediction = results$prediction, Actual = test$RSN1)

# Calculate accuracy for results 
library(caret) 
confusionMatrix(results$prediction, test$RSN1)
