# clear environment variables
rm(list = ls())

library(rpart)
library(rattle)         # Fancy tree plot
library(rpart.plot)
library(party)					# Alternative decision tree algorithm
library(partykit)
library(plyr)
library(ReporteRs)

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
final.dataset$RSN1 <- as.factor(final.dataset$RSN1)
# final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
head(final.dataset, 500)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))

train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

# creating another copy of test dataset for confusion matrix
copy_test <- final.dataset[-train_index, ]

#Classification Tree using Risk Score as predicted variable.
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


# Classification tree on RSN codes
ClassTree2<- rpart(RSN1 ~ TotalChg + Days, method="class", data = denied_claims)
prp(ClassTree2, box.col=c("lightblue"))

# Calculate accuracy for results 
library(caret)
confusionMatrix(x$pred, copy_test$RSN1)


# Create a docx object
# doc = docx()

# add a table
# MyFTable = FlexTable( data = a, add.rownames = TRUE )
# doc = addFlexTable(doc, MyFTable)

# write the doc
# writeDoc( doc, file = "example.docx" )

# open the Word doc
# browseURL("example.docx")

# _____________________ ROCR CURVE __________________________
# library(ROCR)
# pred1 <- prediction(predict(ClassTree1), test$Decision)
# perf1 <- performance(pred, copy_test$Decision)
# plot(perf1)


