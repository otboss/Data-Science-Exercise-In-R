rm(list=ls())
options(scipen=99999)

library(classInt)

# Setting working directory dynamically
tryCatch({
  setwd(getSrcDirectory()[1])
}, error = function (e) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
})

# Loading dataset
dataset <- read.csv("./datasets/SalesData.csv", na.strings = c("", " ", "\"\"", "?", "??", "???", "!"), stringsAsFactors = T)
str(dataset)
summary(dataset)


# #############
# Data Cleaning
# 
# Step 1 - Removal of irrelevant data
# Step 2 - Deduplicate / Aggregate data
# Step 3 - Fix structural errors
# Step 4 - Deal with missing data
# Step 5 - Filter out data outliers
# Step 6 -  Validate data
# 

# NA values for Region identified using Summary
Summary(dataset$Region)

# Replace NA's from Region column
dataset$Region <- as.character(dataset$Region)
dataset$Region[is.na(dataset$Region)] <- "Unknown"
dataset$Region <- factor(dataset$Region)

# NA values for ItemType identified using Summary
Summary(dataset$ItemType)

# Replace NA's from ItemType column
dataset$ItemType <- as.character(dataset$ItemType)
dataset$ItemType[is.na(dataset$ItemType)] <- "Unknown"
dataset$ItemType <- factor(dataset$ItemType)

# NA values for Sales.Channel identified using Summary
Summary(dataset$Sales.Channel)

# Replace NA's and irrelevant values from SalesChannel column
dataset$Sales.Channel <- as.character(dataset$Sales.Channel)
dataset$Sales.Channel[is.na(dataset$Sales.Channel)] <- "Unknown"
dataset$Sales.Channel[dataset$Sales.Channel != "Online" & dataset$Sales.Channel != "Offline"] <- "Unknown"
dataset$Sales.Channel <- factor(dataset$Sales.Channel)
levels(dataset$Sales.Channel)

dataset$Order.Date <- as.Date(dataset$Order.Date, format = "%m/%d/%Y")
dataset$Ship.Date <- as.Date(dataset$Ship.Date, format = "%m/%d/%Y")

# Normalization of unit price field
dataset$Unit.Price <- ((dataset$Unit.Price - min(dataset$Unit.Price))/(max(dataset$Unit.Price) - min(dataset$Unit.Price))) * (10-1) + 1

# Discretization of Quantity (also addresses outliers)
#
# Key:
# L  -> Low
# M  -> Medium
# H  -> High
#
bins <- classIntervals(dataset$Units.Sold, 3, style = 'equal')
unitsSoldType = c()
for (i in 1:length(dataset$Units.Sold)) {
  rating = "L"
  if(dataset$Units.Sold[i] > bins$brks[2] && dataset$Units.Sold[i] <= bins$brks[3]){
    rating = "M"
  }
  if(dataset$Units.Sold[i] > bins$brks[3]){
    rating = "H"
  }
  unitsSoldType <- append(unitsSoldType, rating)
}
dataset$Units.Sold.Type <- as.factor(unitsSoldType)

# Inspect new categorical column
summary(dataset$Units.Sold.Type)

str(dataset$Unit.Price)
hist(dataset$Unit.Price)

# Plot density graph...
plot(density(dataset$Unit.Price, na.rm = T))

# Write cleaned dataset to file
write.csv(dataset, "./output/sales-data_cleaned.csv", row.names = F)

############# DATA CLEANING COMPLETE #############




# Inspect cleaned dataset
View(dataset)

cleanedDataset <- read.csv("./output/sales-data_cleaned.csv", stringsAsFactors = T)





# <------------------ Problem 01 ------------------>

# Problem 01a - Apply binning and normalization as necessary to improve results.

# Completed in data cleaning and preparation process above

# Problem 01b - Construct new field which has the number of days between order Date and Ship Date.

deliveryDays <- c()
for (i in 1:length(cleanedDataset$Order.Date)) {
  shippingDays <- as.Date(cleanedDataset$Ship.Date[i]) - as.Date(cleanedDataset$Order.Date[i])
  deliveryDays <- append(deliveryDays, as.integer(strtoi(shippingDays, base = 0L)))
}
# Assigning to new column called "Ship Time"
cleanedDataset$Ship.Time <- deliveryDays

# Problem 01c - Provide short in-code comments to explain the reason and what choices are made.

# <------------------ End of Problem 01 ------------------>



# Check dataset to confirm changes above
View(cleanedDataset)


# <------------------ Problem 02 ------------------>

cleanedDataset$Order.ID <- NULL

# Convert factors to numeric for clustering
cleanedDataset <- dplyr::mutate_all(cleanedDataset, function(x) as.numeric(x))


sample.salesdata <- cleanedDataset[1:200,]
str(sample.salesdata)


# Hierarchical clustering (Agglomerative)

dis.matrix <- dist(sample.salesdata)
hclust.01 <- hclust(dis.matrix, method = "average")
plot(hclust.01)


# 4 cluster : 
cluster.members.four <- cutree(hclust.01, 4)
plot(cluster.members.four)

aggregate(sample.salesdata, by=list(cluster.members.four), FUN = mean)

# 6 cluster : 
cluster.members.six <- cutree(hclust.01, 6)
plot(cluster.members.six)

aggregate(sample.salesdata, by=list(four.cluster.members), FUN = mean)

# 8 cluster : 
cluster.members.eight <- cutree(hclust.01, 8)
plot(cluster.members.eight)

aggregate(sample.salesdata, by=list(cluster.members.eight), FUN = mean) 


# k-means clustering

# 4 cluster : 
km.results.four <- kmeans(sample.salesdata, 4)
km.results.four
plot(sample.salesdata, km.results.four$cluster)


# 6 cluster : 
km.results.six <- kmeans(sample.salesdata, 6)
km.results.six
plot(sample.salesdata, km.results.six$cluster)

# 8 cluster :
km.results.eight <- kmeans(sample.salesdata, 8)
km.results.eight
plot(sample.salesdata, km.results.eight$cluster)

# <------------------ End of Problem 02 ------------------>

# <------------------ Problem 03 ------------------>
# <------------------ End of Problem 03 ------------------>
