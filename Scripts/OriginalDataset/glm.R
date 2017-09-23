# clear environment variables
rm(list = ls())

library(ggplot2)

# Read train dataset
accepted_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/acceptedClaims_dataset.csv")
denied_claims <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_dataset.csv")

accepted_claims[is.na(accepted_claims)] <- 0
denied_claims[is.na(denied_claims)] <- 0

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

# below line for multiclass approach
# final.dataset$RSN1 <- as.factor(final.dataset$RSN1)
final.dataset$RSN1 <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
head(final.dataset, 500)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(final.dataset))
train_index <- sample(seq_len(nrow(final.dataset)), size = smp_size)

train <- final.dataset[train_index, ]
test <- final.dataset[-train_index, ]

head(train)
head(test)

# train$RSN1 <- NULL
# test$RSN1 <- NULL
head(train)

fit <- glm(RSN1 ~ . , data = train, family = "binomial")

summary(fit)

test.pred <- predict(fit, test, type = "response")
test.pred.Decision <- rep(200, nrow(test))
test.pred.Decision[test.pred > 0.5] <- 100

# Confusion matrix
table( Prediction = test.pred.Decision, ActualValue = test$RSN1)

summary(test.pred)

# Calculate accuracy for results 
library(caret)
confusionMatrix(test.pred.Decision, test$RSN1)

ggplot(fit, aes(x=Days, color=RSN1, linetype=RSN1)) + geom_density() ## don't use it as of now

# ggplot(train, aes(Days, linetype = Decision, color = Decision)) + geom_density()

# ggplot(final.dataset, aes(TotalChg, colour = Decision)) +
# geom_freqpoly(binwidth = 15000) + facet_wrap(~ Decision) + scale_colour_brewer(palette = "Set1")


# ggplot(final.dataset, aes(Days, colour = Decision, fill = Decision)) +
#   geom_histogram() + facet_wrap(~ Decision) + scale_colour_brewer(palette = "Set1")

# ggplot(final.dataset, aes(TotalChg, colour = Decision)) +
#   geom_histogram() + facet_wrap(~ Decision) + scale_colour_brewer(palette = "Set1")

# ggplot(train, aes(Days, colour = Decision)) +
#   geom_histogram(aes(fill = Decision)) + facet_wrap(~ Decision) 

# ggplot(final.dataset, aes(y = TotalChg,x = Days, colour = Decision)) +
#  geom_point() + facet_wrap(~ Decision) + scale_colour_brewer(palette = "Set1")
# 
