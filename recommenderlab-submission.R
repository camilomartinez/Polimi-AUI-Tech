library(recommenderlab)
library(plyr)

source("modified-packages.R")
source("custom-functions.R")

# SUbmission with association rules

#Preparing training and test data
trainCsv = ReadCsvData("train")
# Coerce to the class used by recommenderlab
urm <- as(trainCsv, "realRatingMatrix")
# Binarize just according to ratings
#urmBinary <- binarize(urm, minRating=1)
# Train recommender
recommender <- Recommender(urm, method = "AR", param=list(minRating=1))
popularRecommender <- Recommender(urm, method = "POPULAR")
# Generate recommendations
testCsv <- ReadCsvData("test")
testUserIds <- testCsv$UserId
recommendations <- predict(recommender, testUserIds, data=urm, n=5)
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
                                              data=urm, n=5)
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
