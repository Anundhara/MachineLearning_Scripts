library(plyr)
library(dplyr)
library(ReporteRs)

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

# conversion of character to numeric values
large.data$TotalChg <- as.numeric(large.data$TotalChg)
large.data$TotalPay <- as.numeric(large.data$TotalPay)

denied.data$TotalChg <- as.numeric(denied.data$TotalChg)
denied.data$TotalPay <- as.numeric(denied.data$TotalPay)

head(large.data)
# head(large.data1)

head(denied.data)
# head(denied.data1)

## set the seed to make your partition reproductible
# set.seed(123)

# samples from accepted and denied claims datasets
m <- large.data[sample(nrow(large.data), 10000, rep = FALSE), ]
n <- denied.data[sample(nrow(denied.data), 3000, rep = FALSE), ]

# combine the two datasets
total <- rbind.fill(m,n)

# shuffle row-wise 
final.dataset <- total[sample(nrow(total)),]

b <- as.data.frame(sort(table(final.dataset$ProviderName), decreasing = TRUE))
a <- sort(table(final.dataset$PayerName), decreasing = TRUE)
a <- as.data.frame(a)
c <- as.data.frame(sort(table(final.dataset$ICN_DCN), decreasing = TRUE))
head(c)
nrow(c)
table(final.dataset$BatchID)
table(final.dataset$PatientName)

# unique(final.dataset$ICN_DCN)
x <- as.data.frame(as.factor(final.dataset$ICN_DCN))
head(x, 500)
nrow(x)
colnames(x)[1] <- "total"

y <- x %>% group_by(total) %>% tally()
y <- as.data.frame(y)
nrow(y)
# y <- table(y)
# head(y)
write.csv(y, "final.csv")

# Create a docx object
# doc = docx()

# add a table
# MyFTable = FlexTable( data = c, add.rownames = TRUE )
# doc = addFlexTable(doc, MyFTable)

# write the doc
# writeDoc( doc, file = "example.docx" )
# open the Word doc
# browseURL("example.docx")

