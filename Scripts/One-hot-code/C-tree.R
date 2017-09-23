# This rule prepares the denied claims dataset and accepted claims dataset. 
# Special characters are eliminated
# Total number of days is calculated from payment date and date of service 
# Conversion of character to numeric type

# clear environment variables
rm(list = ls())

library(rpart)
library(rpart.plot)

# read accepted claims dataset
accepted.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_synthetic_dataset.csv", sep=",", na = "0")

# read denied claims dataset
denied.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_synthetic_dataset.csv", sep=",", na = "0")

accepted.data[is.na(accepted.data)] <- 0
denied.data[is.na(denied.data)] <- 0

# Replace all negative values by zero
accepted.data$Days <- ifelse(accepted.data$Days < 0, 0, accepted.data$Days)
denied.data$Days <- ifelse(denied.data$Days < 0, 0, denied.data$Days)

#dummy coding section - for denied claims dataset
denied.data$CO58 <- ifelse(denied.data$RSN1 == 58,'Yes', 'No')
denied.data$CO15 <- ifelse(denied.data$RSN1 == 15,'Yes', 'No')
denied.data$CO22 <- ifelse(denied.data$RSN1 == 22,'Yes', 'No')
denied.data$CO29 <- ifelse(denied.data$RSN1 == 29,'Yes', 'No')
denied.data$CO45 <- ifelse(denied.data$RSN1 == 45,'Yes', 'No')
denied.data$CO97 <- ifelse(denied.data$RSN1 == 97,'Yes', 'No')

#dummy coding section - for accepted claims dataset

accepted.data$CO58 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')
accepted.data$CO15 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')
accepted.data$CO22 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')
accepted.data$CO29 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')
accepted.data$CO45 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')
accepted.data$CO97 <- ifelse(accepted.data$RSN1 == 100,'No', 'Yes')

head(accepted.data, 100)
head(denied.data, 100)


# samples from accepted and denied claims datasets
m <- accepted.data[sample(nrow(accepted.data), 10000, rep = FALSE), ]
n <- denied.data[sample(nrow(denied.data), 8000, rep = FALSE), ]

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
# final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
final.dataset$Marker <- NULL

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

final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))

train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

# creating another copy of test dataset for confusion matrix
copy_test <- final.dataset[-train_index, ]

#Classification Tree using Risk Score as predicted variable.
# ClassTree1<- rpart(RSN1 ~ Likely45 + Likely29 + TotalChg + Days , method="class", data = train)
ClassTree1<- rpart(RSN1 ~ ., method="class", data = train)


summary(ClassTree1)

prp(ClassTree1, box.col=c("lightblue"))
# prp(ClassTree1)

# apply model to test set
# Confusion matrix
pred <- predict(ClassTree1, test, type = "class")
x <- as.data.frame(pred)
table(Prediction = x$pred, Actual = copy_test$RSN1)
summary(pred)

# Calculate accuracy for results 
library(caret)
confusionMatrix(x$pred, copy_test$RSN1)





