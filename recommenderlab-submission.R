library(recommenderlab)
library(plyr)

source("custom-functions.R")

# SUbmission with association rules

#Preparing training and test data
trainCsv = read.csv("data/train.csv")
# Coerce to the class used by recommenderlab
urm <- as(trainCsv, "realRatingMatrix")
# Binarize just according to ratings
urmBinary <- binarize(urm, minRating=1)
# Train recommender
recommender <- Recommender(urmBinary, method = "AR")
popularRecommender <- Recommender(urmBinary, method = "POPULAR")
# Generate recommendations
testCsv <- read.csv("data/test.csv")
testUserIds <- testCsv$UserId
recommendations <- predict(recommender, testUserIds, data=urmBinary, n=5)
as(recommendations,"list")
# Aggregate per user
recommendedPerUser <- data.frame(UserId = testUserIds)
recommendedPerUser$ItemIds <- as(recommendations,"list")

# Prepare as string
submission <- ddply(recommendedPerUser, "UserId", 
                    function(userRow) {
                      # Retrieve existing recommendations
                      itemIds <- GetItemIdsVector(userRow)
                      numberOfExistingRecommendations <- length(itemIds)
                      desiredNumberRecommendations <- 5
                      numberOfMissingRecommendations <-  desiredNumberRecommendations - numberOfExistingRecommendations
                      if(numberOfMissingRecommendations == 0)
                      {
                        # Recommendations are complete
                        recommended <- itemIds
                      } else {
                        # Fill missing recommendations with popular
                        topPopular <- predict(popularRecommender, userRow$UserId,
                                              data=urmBinary, n=5)
                        # Convert from topNList to vector
                        topPopular <- as(topPopular, "list")[[1]]
                        newRecommendations <- head(topPopular, numberOfMissingRecommendations)
                        recommended <- append(itemIds, newRecommendations)
                      }
                      RecommendedMovieIds <- paste(recommended, collapse = " ")
                      data.frame(RecommendedMovieIds)
                    })
# Write to file
WriteSubmission(submission)
