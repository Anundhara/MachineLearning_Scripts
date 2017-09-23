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

# clean the columns
large.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalChg)
large.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", large.data$TotalPay)

denied.data$TotalChg <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalChg)
denied.data$TotalPay <- gsub("\\(|\\)|\\$|\\,", "", denied.data$TotalPay)
denied.data$PatientName <- gsub("\\,", "", denied.data$PatientName)

# conversion of character to numeric values
large.data$TotalChg <- as.numeric(large.data$TotalChg)
large.data$TotalPay <- as.numeric(large.data$TotalPay)
large.data$PatientCtrlNum <- as.numeric(large.data$PatientCtrlNum)
large.data$UploadDate <- as.numeric(large.data$UploadDate)

denied.data$TotalChg <- as.numeric(denied.data$TotalChg)
denied.data$TotalPay <- as.numeric(denied.data$TotalPay)
denied.data$PatientCtrlNum <- as.numeric(denied.data$PatientCtrlNum)
denied.data$UploadDate <- as.numeric(denied.data$UploadDate)

# select required fields from denial.data dataframe
# large.data1 <- large.data[, c(2, 3, 4, 12, 13, 15, 16)]
large.data1 <- large.data[, c(12, 13, 15, 16)]
# large.data1 <- large.data[,c(3, 12, 13, 15, 16)]

# denied.data1 <- denied.data[, c(2 ,3, 4, 12, 13, 15, 21)]
denied.data1 <- denied.data[, c(12, 13, 15, 21)]
# denied.data1 <- denied.data[,c(3, 12, 13, 15, 21)]

head(large.data)
head(large.data1)

head(denied.data)
head(denied.data1)

# write.csv(large.data1, "acceptedClaims_dataset.csv")
# write.csv(denied.data1, "deniedClaims_dataset.csv")

# samples from accepted and denied claims datasets
m <- large.data[sample(nrow(large.data), 10000, rep = FALSE), ]
n <- denied.data[sample(nrow(denied.data), 6000, rep = FALSE), ]

n$RSN2 <- NULL
n$RSN3 <- NULL
n$VRSN1 <- NULL
n$VRSN2 <- NULL
n$VRSN3 <- NULL

# combine the two datasets
total <- rbind(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]
# final.dataset$PatientName <- gsub(" ", "_", final.dataset$PatientName)
# write.csv(final.dataset, "synthetic_dataset_trail.csv")

TotalChg <- final.dataset$TotalChg

length(TotalChg)

summary(TotalChg)

TotalChg <- TotalChg[!is.na(TotalChg)]

n <- length(TotalChg)

mean.TotalChg <- mean(TotalChg)
var.TotalChg <- var(TotalChg)
sd.TotalChg <- sd(TotalChg)

# set n points in the interval (0,1)
# use the formula k/(n+1), for k = 1,..,n
# this is a vector of the n probabilities
probabilities = (1:n)/(n+1)

# calculate normal quantiles using mean and standard deviation from "ozone"
normal.quantiles = qnorm(probabilities, mean(TotalChg, na.rm = T), sd(TotalChg, na.rm = T))

plot(sort(normal.quantiles), sort(TotalChg) , xlab = 'Theoretical Quantiles from Normal Distribution', ylab = 'Sample Quantiles', main = 'Normal Quantile-Quantile Plot')
abline(0,1)

y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))

qqnorm(TotalChg)
qqline(TotalChg, col = 2)
