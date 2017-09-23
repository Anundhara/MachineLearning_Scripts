# clear environment variables
rm(list = ls())

#library 
library(rpart)
library(rpart.plot) #for prp function

# Read train dataset
# accepted_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_dataset.csv")
# denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_dataset.csv")

#dummy coding section - for denied claims dataset
# denied_claims$CO58 <- ifelse(denied_claims$RSN1 == 58,'Yes', 'No')
# denied_claims$CO15 <- ifelse(denied_claims$RSN1 == 15,'Yes', 'No')
# denied_claims$CO22 <- ifelse(denied_claims$RSN1 == 22,'Yes', 'No')
# denied_claims$CO29 <- ifelse(denied_claims$RSN1 == 29,'Yes', 'No')
# denied_claims$CO45 <- ifelse(denied_claims$RSN1 == 45,'Yes', 'No')
# denied_claims$CO97 <- ifelse(denied_claims$RSN1 == 97,'Yes', 'No')

#dummy coding section - for accepted claims dataset

# accepted_claims$CO58 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# accepted_claims$CO15 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# accepted_claims$CO22 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# accepted_claims$CO29 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# accepted_claims$CO45 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# accepted_claims$CO97 <- ifelse(accepted_claims$RSN1 == 100,'No', 'Yes')
# 
# head(accepted_claims, 100)
# head(denied_claims, 100)

#decision tree
# samples from accepted and denied claims datasets
# m <- accepted_claims[sample(nrow(accepted_claims), 7000, rep = FALSE), ]
# n <- denied_claims[sample(nrow(denied_claims), 8000, rep = FALSE), ]
# 
# # combine the two datasets
# total <- rbind(m,n)
# 
# # shuffle row-wise 
# final.dataset <- total[sample(nrow(total)),]
# # final.dataset$RSN1 <- as.factor(final.dataset$RSN1)
# #resetting RSN != 100 to 200
# # final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100,200, 0))
# # final.dataset[,final.dataset$RSN1] != 100 <- 200
# 
# write.csv(final.dataset, file = 'test.csv')

#read dummy coded dataset
final.dataset <- read.csv("C:/Users/anunchim/Desktop/test.csv")


## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))

train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

#Classification Tree using Risk Score as predicted variable.
ClassTree1<- rpart(RSN1 ~ CO58 + CO15 + CO22 + CO45 + CO97 + TotalChg + Days , method="class", data = train)

summary(ClassTree1)

prp(ClassTree1, box.col=c("lightblue"))
# prp(ClassTree1)

# apply model to test set
# Confusion matrix
pred <- predict(ClassTree1, test, type = "class")
x <- as.data.frame(pred)
table(Prediction = x$pred, Actual = test$RSN1)
summary(pred)


#glm
as.factor(train$RSN1)
# fit <- glm(RSN1 ~ TotalChg + TotalPay + Days, data = train, family = "binomial")
fit <- glm(RSN1 ~ ., data = train, family = "binomial")
summary(fit)

test.pred <- predict(fit, test, type = "response")
test.pred.Decision <- rep(200, nrow(test))
test.pred.Decision[test.pred > 0.5] <- 100

# Confusion matrix
table( Prediction = test.pred.Decision, ActualValue = test$RSN1)


#SVM
library(e1071)
library(plyr)
library(kernlab)
# svm implementation
svm_model <- svm(RSN1 ~ ., data = train, type='C-classification', kernel='linear')
svm_model1 <- svm(RSN1 ~ ., data = train)
summary(svm_model)
plot(svm_model, train,  TotalChg ~ Days)
plot(svm_model1, train,  TotalChg ~ Days)
pred <- predict(svm_model, test)
table(pred, test$RSN1)

#Neural Network
library(neuralnet)
library(ReporteRs)
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
