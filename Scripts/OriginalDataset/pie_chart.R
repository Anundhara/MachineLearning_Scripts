# clear environment variables
rm(list = ls())

# read denied claims dataset
denied.data <- read.csv("C:/Users/anunchim/Documents/R/Claims_Denial_Analytics/Datasets/deniedClaims_dataset.csv")

one <- sum(denied.data$RSN1 == 15)
two <- sum(denied.data$RSN1 == 22)
three <- sum(denied.data$RSN1 == 29)
four <- sum(denied.data$RSN1 == 45)
five <- sum(denied.data$RSN1 == 97)
six <- sum(denied.data$RSN1 == 58)
# Calculate the percentage for each Paid.Amt 
#  rounded to one decimal place
total <- as.double(c(one, two, three, four, five, six))
percent.label <- round(total/sum(total) * 100)

# Concatenate a '%' char after each value
percent.label <- paste(percent.label, "%", sep="")

LegendValues <- c("CO 15", "CO 22", "CO 29", "CO 45", "CO 58", "CO 97") 
slices <- c(one , two, three, four, five, six)

pie(slices,labels=percent.label, main="Frequencies of Error Reason Codes", col=rainbow(length(slices)))
legend("topright",LegendValues, fill = rainbow(length(slices)))

table(denied.data$RSN1)
