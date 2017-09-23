# clear environment variables
rm(list = ls())

library(e1071)
library(plyr)
library(kernlab)

# read accepted claims dataset
accepted_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_synthetic_dataset.csv")

# read denied claims dataset
denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_synthetic_dataset.csv")


# Replace all negative values by zero
accepted_claims$Days <- ifelse(accepted_claims$Days < 0, 0, accepted_claims$Days)
denied_claims$Days <- ifelse(denied_claims$Days < 0, 0, denied_claims$Days)

accepted_claims[is.na(accepted_claims)] <- 0
denied_claims[is.na(denied_claims)] <- 0

head(accepted_claims)
head(denied_claims)

## set the seed to make your partition reproductible
# set.seed(123)

#dummy coding section - for denied claims dataset
denied_claims$CO58 <- ifelse(denied_claims$RSN1 == 58,'Yes', 'No')
denied_claims$CO15 <- ifelse(denied_claims$RSN1 == 15,'Yes', 'No')
denied_claims$CO22 <- ifelse(denied_claims$RSN1 == 22,'Yes', 'No')
denied_claims$CO29 <- ifelse(denied_claims$RSN1 == 29,'Yes', 'No')
denied_claims$CO45 <- ifelse(denied_claims$RSN1 == 45,'Yes', 'No')
denied_claims$CO97 <- ifelse(denied_claims$RSN1 == 97,'Yes', 'No')

#dummy coding section - for accepted claims dataset

accepted_claims$CO58 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
accepted_claims$CO15 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
accepted_claims$CO22 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
accepted_claims$CO29 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
accepted_claims$CO45 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
accepted_claims$CO97 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')

head(accepted_claims, 100)
head(denied_claims, 100)
# samples from accepted and denied claims datasets
m <- accepted_claims[sample(nrow(accepted_claims), 10000, rep = FALSE), ]
n <- denied_claims[sample(nrow(denied_claims), 8000, rep = FALSE), ]

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
head(final.dataset)

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

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))

train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

head(train)
head(test)

test_result <- test$RSN1

# svm implementation
svm_model <- svm(RSN1 ~ ., data = train, type='C-classification', kernel='linear')
svm_model1 <- svm(RSN1 ~ ., data = train)
summary(svm_model)

plot(svm_model, train,  TotalChg ~ Days)
plot(svm_model1, train,  TotalChg ~ Days)

# Confusion matrix
pred <- predict(svm_model, test)
table(pred, test_result)

# Calculate accuracy for results 
library(caret)
confusionMatrix(pred, test_result)
