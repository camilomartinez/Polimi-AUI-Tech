# Load required library
library(recommenderlab)

source("custom-functions.R")

#Preparing training and test data
trainCsv = ReadCsvData("train")
# Filtering out test ratings
testUserIds = read.csv("data/test.csv")
nonTestRatings <- trainCsv[! trainCsv$UserId %in% testUserIds$UserId,]
# Coerce to the class used by recommenderlab
urm <- as(nonTestRatings, "realRatingMatrix")

# Binarize over 4 to leave relevant items only
urmBin <- binarize(urm, 4)
# Count how many relevant items per user
relevantCountPerUser <- rowCounts(urmBin)
# number of recommendation / N for each user
relevantInverse <- 5 / relevantCountPerUser
# Allocate index and vector replacement
noRelevantItems <- relevantCountPerUser == 0
zeroVector <- rep(0,length(relevantCountPerUser))
# Replace infinite entries for 0
relevantInverse[noRelevantItems] <- zeroVector[noRelevantItems]
# Compute mean average
perfectMapOnTrain <- mean(relevantInverse)
perfectMapOnTrain
