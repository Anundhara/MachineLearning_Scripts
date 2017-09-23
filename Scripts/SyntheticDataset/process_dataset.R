# This rule prepares the denied claims dataset and accepted claims dataset. 
# Special characters are eliminated
# Total number of days is calculated from payment date and date of service 
# Conversion of character to numeric type

# clear environment variables
rm(list = ls())

# read accepted claims dataset
large.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/OS45csv.csv", sep=",", na = "0")

# read denied claims dataset
denied.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/DeniedClaimsMergev1_FINAL.csv", sep=",", na = "0")

# calculate number of days between service provided and payment date
large.data$Days <- as.Date(as.character(large.data$PaymentDate), format="%m/%d/%Y")-
  as.Date(as.character(large.data$DOS), format="%m/%d/%Y")

denied.data$Days <- as.Date(as.character(denied.data$PaymentDate), format="%m/%d/%Y")-
  as.Date(as.character(denied.data$DOS), format="%m/%d/%Y")

large.data$Days <- as.numeric(large.data$Days)
denied.data$Days <- as.numeric(denied.data$Days)

# Replace all negative values by zero
large.data$Days <- ifelse(large.data$Days < 0, 0, large.data$Days)
denied.data$Days <- ifelse(denied.data$Days < 0, 0, denied.data$Days)

# clean the columns
large.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalChg)
large.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalPay)

denied.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalChg)
denied.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalPay)

# conversion of character to numeric values
large.data$TotalChg <- as.numeric(large.data$TotalChg)
large.data$TotalPay <- as.numeric(large.data$TotalPay)

denied.data$TotalChg <- as.numeric(denied.data$TotalChg)
denied.data$TotalPay <- as.numeric(denied.data$TotalPay)

# select required fields from denial.data dataframe
# large.data1 <- large.data[, c(2, 3, 4, 12, 13, 15, 16)]
large.data1 <- large.data[, c(12, 13, 15, 16)]
# large.data1 <- large.data[,c(3, 12, 13, 15, 16)]

# denied.data1 <- denied.data[, c(2 ,3, 4, 12, 13, 15, 21)]
denied.data <- denied.data[, c(12, 13, 15, 21)]
# denied.data1 <- denied.data[,c(3, 12, 13, 15, 21)]

head(large.data)
head(large.data1)

head(denied.data)
# head(denied.data1)

write.csv(large.data1, "acceptedClaims_dataset.csv")
write.csv(denied.data, "deniedClaims_dataset.csv")

