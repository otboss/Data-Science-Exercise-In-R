# install.packages("pacman")

# Load packages, install if unavailable
pacman::p_load(
  "arules",
  # "arulesViz",
  "zeallot",
  "backports",
  "classInt",
  "dplyr",
  "chron"
)

rm(list=ls())
options(scipen=99999)

# Setting working directory dynamically
tryCatch({
  setwd(getSrcDirectory()[1])
}, error = function (e) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
})

# Loading dataset
dataset <- read.csv("./dataset/OnlineRetail.csv", na.strings = c("", " ", "\"\"", "?", "??", "???", "!"), stringsAsFactors = T)
str(dataset)
summary(dataset)


# #############
# Data Cleaning
# 
# Step 1 - Removal of irrelevant data
# Step 2 - Deduplicate / Aggregate your data
# Step 3 - Fix structural errors
# Step 4 - Deal with missing data
# Step 5 - Filter out data outliers
# Step 6 -  Validate your data
# 

# Removing irrelevant data (noise removal)
dataset <- filter(dataset, Country == "Switzerland")

# Checking and removal for meaningless descriptions
emptyDescriptions <- dataset[nchar(as.character(dataset$Description)) <= 5, "Description"]
emptyDescriptions
length(emptyDescriptions)
dataset <- filter(dataset, nchar(as.character(dataset$Description)) > 5)

# Fixing items with a negative quantity
dataset$Quantity <- abs(dataset$Quantity)
summary(dataset$Description)

# Discretization of Quantity (also addresses outliers)
#
# Key:
# L -> Low
# M -> Medium
# H -> High
# VH -> Very High
#
dataset$Quantity <- cut(dataset$Quantity, c(0,1,3,8,max(dataset$Quantity)), right = TRUE, labels = c("L","M","H","VH"))
summary(dataset$Quantity)

# replacement of unwanted characters
dataset$Description <- trimws(dataset$Description)

# replace all spaces with underscores in description
dataset$Description <- gsub(" ", "_", dataset$Description)

# Removing free items
dataset <- filter(dataset, UnitPrice > 0)

# replace "C" spaces with nothing in InvoiceNo
dataset$InvoiceNo <- gsub("C", "", dataset$InvoiceNo)

# cleaning date field (separation of date and time)
invoiceTimes <- c()
invoiceDates <- c()
for(i in 1:length(dataset$InvoiceDate)){
  splittedDate <- strsplit(toString(dataset$InvoiceDate[i]), " ")
  invoiceDate <- splittedDate[[1]][1]
  # Seconds are needed in order to convert string to chron objects
  invoiceTime <- paste(splittedDate[[1]][2], ":00", sep="")
  invoiceTimes <- append(invoiceTimes, invoiceTime)
  invoiceDates <- append(invoiceDates, invoiceDate)
}
# Override invoice dates with standardized format
dataset$InvoiceDate <- invoiceDates


# converting dates to native r dates using chron
invoiceDates <- as.Date(dataset$InvoiceDate, format = "%m/%d/%Y")

# creating cron objects
chronDateValues <- chron(date=as.character(invoiceDates), times=invoiceTimes, format=c(dates="Y-m-d", times="h:m:s"))

# Add chron dates as new column after "InvoiceDate" column
dataset <- tibble::add_column(dataset, ChronDates = chronDateValues, .after = "InvoiceDate")

# View cleaned dataset
View(dataset)

# Output cleaned dataset to file
write.csv(dataset, "./output/switzerland-salesdata_cleaned.csv", row.names = F)

############# DATA CLEANING COMPLETE #############




# Association Rules

# <------------------ Problem 02 ------------------>


# Goal: 
# to identify the settings for Minimum Support and Confidence
# and present the extracted association rules for best performing associations

# Loading in cleaned dataset
switzerlandSalesData <- read.transactions("./output/switzerland-salesdata_cleaned.csv", format=c("single"), header = TRUE, rm.duplicates = FALSE, cols = c("InvoiceNo", "StockCode"), sep=",")

# Viewing the item frequencies
itemFrequencyPlot(switzerlandSalesData, support=0.1)
itemFrequencyPlot(switzerlandSalesData, topN=4)


drules <- apriori(dataset)
summary(drules)
inspect(drules)
View(as(drules, "data.frame"))

drules2 <- apriori(dataset, parameter = list(support=0.2, confidence=0.75, minlen=2))
summary(drules2)
inspect(drules2)
View(as(drules, "data.frame"))

subsetRules <- results[quality(results)$lift>1]
inspect(subsetRules)


plot(drules2[1:4], method = "graph")



itemFrequencyPlot(dataset$Description, support=0.2)


# <------------------ End of Problem 02 ------------------>





