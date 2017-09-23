# clear environment variables
rm(list = ls())

library(e1071)
library(plyr)
library(kernlab)

# Read train dataset
accepted_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_dataset.csv")
denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_dataset.csv")

accepted_claims[is.na(accepted_claims)] <- 0
denied_claims[is.na(denied_claims)] <- 0

head(accepted_claims)
head(denied_claims)

## set the seed to make your partition reproductible
# set.seed(123)

# samples from accepted and denied claims datasets
m <- accepted_claims[sample(nrow(accepted_claims), 10000, rep = FALSE), ]
n <- denied_claims[sample(nrow(denied_claims), 3000, rep = FALSE), ]

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
# below line for multiclass approach
final.dataset$RSN1 <- as.factor(final.dataset$RSN1)
# final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
head(final.dataset, 500)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))

train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

head(train)
head(test)

test_result <- test$RSN1
test$Decision <- NULL

# svm implementation
svm_model <- svm(RSN1 ~ ., data = train, type='C-classification', kernel='linear')
svm_model1 <- svm(RSN1 ~ ., data = train)

summary(svm_model)

plot(svm_model, train, TotalChg ~ Days)
plot(svm_model1, train, TotalChg ~ Days)
# plot(svm_model, train,  Decision ~ TotalChg)

# Confusion matrix
pred <- predict(svm_model, test)
table(pred, test_result)

# Calculate accuracy for results 
library(caret)
confusionMatrix(pred, test_result)
