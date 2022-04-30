
library(classInt)

rm(list=ls())
options(scipen=99999)

# Setting working directory dynamically
tryCatch({
  setwd(getSrcDirectory()[1])
}, error = function (e) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
})

# Loading dataset
dataset <- read.csv("./dataset/SalesData.csv", na.strings = c("", " ", "\"\"", "?", "??", "???", "!"), stringsAsFactors = T)
str(dataset)


# 
# Data Cleaning
# 
# Step 1 - Removal of irrelevant data
# Step 2 - Deduplicate / Aggregate data
# Step 3 - Fix structural errors
# Step 4 - Deal with missing data
# Step 5 - Filter out data outliers
# Step 6 -  Validate data
# 

# TODO: Remove all NA's
summary(dataset$Region)
dataset$Region[is.na(dataset$Region)] <- "unknown"
levels(dataset$Region)

summary(dataset$ItemType)

summary(dataset$Sales.Channel)
levels(dataset$Sales.Channel)[3] <- NA
summary(dataset$Sales.Channel)
dataset$Sales.Channel[is.na(dataset$Sales.Channel)] <- "unknown"


summary(dataset$Order.Priority)
summary(dataset$Order.Date)
summary(dataset$Order.ID)

str(dataset$Unit.Price)
hist(dataset$Unit.Price)
plot(density(dataset$Unit.Price, na.rm = T))

View(dataset)


# <------------------ Problem 01 ------------------>


# Problem 01a - Apply binning and normalization as necessary to improve results.

dataset$Unit.Price <- normalize(dataset$Unit.Price)

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
dataset$Units.Sold.Type <- unitsSoldType

str(dataset$Units.Sold.Type)

# Problem 01b - Construct new field which has the number of days between order Date and Ship Date.

deliveryDays <- c()
for (i in 1:length(dataset$Order.Date)) {
  shippingDays <- as.Date(dataset$Ship.Date[i], format = "%m/%d/%Y") - as.Date(dataset$Order.Date[i], format = "%m/%d/%Y")
  deliveryDays <- append(deliveryDays, strtoi(shippingDays, base = 0L))
}
dataset$Ship.Time <- deliveryDays

# Problem 01c - Provide short in-code comments to explain the reason and what choices are made.
# TODO

# <------------------ End of Problem 01 ------------------>

View(dataset)
# Convert factors to numeric for clustering
dataset$Region <- as.numeric(dataset$Region)
dataset$Units.Sold <- as.numeric(dataset$Units.Sold)
dataset$Unit.Price <- as.numeric(dataset$Unit.Price)
dataset$Order.Priority <- as.numeric(dataset$Order.Priority)
# dataset$Region <- as.numeric(dataset$Region)
# dataset$Region <- as.numeric(dataset$Region)
# dataset$Region <- as.numeric(dataset$Region)




# <------------------ Problem 02 ------------------>
length(dataset$Units.Sold.Type)

sample.salesdata <- dataset[1:1000,]
str(sample.salesdata)


# Hierarchical clustering (Agglomerative)

dis.matrix <- dist(sample.salesdata)
hclust.01 <- hclust(dis.matrix, method = "average")
plot(hclust.01)


# 4 cluster : 
cluster.members.four <- cutree(hclust.01, 4)
View(as.data.frame(cluster.members.four))

aggregate(sample.salesdata, by=list(cluster.members.four), FUN = mean)

# 6 cluster : 
cluster.members.six <- cutree(hclust.01, 6)
View(as.data.frame(cluster.members.six))

aggregate(sample.salesdata, by=list(four.cluster.members), FUN = mean)

# 8 cluster : 
cluster.members.eight <- cutree(hclust.01, 8)
View(as.data.frame(cluster.members.eight))

aggregate(sample.salesdata, by=list(cluster.members.eight), FUN = mean) 


# k-means clustering

# 4 cluster : 
km.results.four <- kmeans(sample.salesdatam, 4)
km.results.four
plot(sample.salesdata, km.results.four$cluster)


# 6 cluster : 
km.results.six <- kmeans(sample.salesdatam, 6)
km.results.six
plot(sample.salesdata, km.results.six$cluster)

# 8 cluster :
km.results.eight <- kmeans(sample.salesdatam, 8)
km.results.eight
plot(sample.salesdata, km.results.eight$cluster)

# <------------------ End of Problem 02 ------------------>

# <------------------ Problem 03 ------------------>
# <------------------ End of Problem 03 ------------------>
