# clear environment variables
rm(list = ls())

# association rules library
library("arules")
# library for visualization
library("arulesViz")
# to combine two dataframes, rbind.fill()
library(plyr)

# read accepted claims dataset
large.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/OS45csv.csv", sep=",", na = "0")

# calculate number of days between service provided and payment date
large.data$Days <- as.Date(as.character(large.data$PaymentDate), format="%m/%d/%Y")-
  as.Date(as.character(large.data$DOS), format="%m/%d/%Y")

# clean the columns
large.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalChg)
large.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalPay)

# conversion of character to numeric values
large.data$TotalChg <- as.numeric(large.data$TotalChg)
large.data$TotalPay <- as.numeric(large.data$TotalPay)

# select required fields from denial.data dataframe
large.data1 <- large.data[, c(2, 4, 12, 13, 15, 16)]

head(large.data)
head(large.data1)

# read denied claims dataset
denied.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/DeniedClaimsMergev1_FINAL.csv", sep=",", na = "0")

# calculate number of days between service provided and payment date
denied.data$Days <- as.Date(as.character(denied.data$PaymentDate), format="%m/%d/%Y")-
  as.Date(as.character(denied.data$DOS), format="%m/%d/%Y")

# clean the columns
denied.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalChg)
denied.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalPay)

# conversion of character to numeric values
denied.data$TotalChg <- as.numeric(denied.data$TotalChg)
denied.data$TotalPay <- as.numeric(denied.data$TotalPay)

# select required fields from denial.data dataframe
denied.data1 <- denied.data[, c(2, 4, 12, 13, 15, 21)]

head(denied.data1)

set.seed(100)

# samples from accepted and denied claims datasets
m <- large.data1[sample(nrow(large.data1), 8000, rep = FALSE), ]
n <- denied.data1[sample(nrow(denied.data1), 3000, rep = FALSE), ]

# combine the two datasets
total <- rbind.fill(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
head(final.dataset)

# apply association rules
final.dataset$TotalChg <- discretize(final.dataset$TotalChg, method="frequency", categories = 10,labels = NULL,     
                                     ordered=FALSE, onlycuts=FALSE)

final.dataset$TotalPay <- discretize(final.dataset$TotalPay, method="frequency", categories = 10, labels = NULL,     
                                     ordered=FALSE, onlycuts=FALSE)

final.dataset$PatientCtrlNum <- as.factor(final.dataset$PatientCtrlNum)
final.dataset$RSN1 <- as.factor(final.dataset$RSN1)
final.dataset$Days <- as.factor(final.dataset$Days)

final.dataset$Decision <- as.factor(ifelse(final.dataset$RSN1 == 100, 100, 200))
final.dataset$RSN1 <- NULL
head(final.dataset)

trans <- as(final.dataset, "transactions")

rules = apriori(trans, parameter=list(support=0.01, confidence=0.5))
inspect(rules)
inspect(head(sort(rules, by="lift"),20))

rules <- apriori(trans, 
                 parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c("Decision=100", "Decision=200"),
                                   default="lhs"),
                 control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
rules1 <- head(sort(rules.pruned, by="lift"),10)

plot(rules)
plot(rules.pruned)
plot(rules1)

plot(rules, method="graph", control=list(type="items"))
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules1, method="graph", control=list(type="items"))

plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
plot(rules1, method="paracoord", control=list(reorder=TRUE))
