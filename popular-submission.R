# For aggregation
library(plyr)
# For evaluation
library("Metrics")
# Custom common functions
source("custom-functions.R")

# Configurations constants
# Wheter we are evaluating or submitting results
evaluation = FALSE
kRemoveSeen = ! evaluation
kSubmission = ! evaluation
kNumberOfRecommendations = 5

# Complete training user rating matrix
urm <- LoadURM()

# Recommendation functions
recommend <- PopularRecommender(urm, kRemoveSeen, kNumberOfRecommendations)

if (evaluation) {
  seenItems = ItemsSeenByNonTestUsers(urm)
} else {
  seenItems = ItemsSeenByTestUsers(urm)
}

# Generate a recommendation per user
recommendedPerUser <- GenerateRecommendations(seenItems, recommend ,kSubmission)

if (evaluation) {
  #Evaluation with optimistic MAP
  actual = dlply(seenItems, "UserId", GetItemIdsVector)
  predicted = dlply(recommendedPerUser, "UserId", GetItemIdsVector)
  # Mean average precision from metrics package
  mapk(kNumberOfRecommendations,actual, predicted)  
} else {
  # submission in output format
  # Output format
  submission <- recommendedPerUser
  # Drop vector of Ids for the output
  submission$ItemIds <- NULL
  write.table(submission, 
              file="submissions/submission.csv", 
              sep=",", quote=FALSE, 
              row.names = FALSE)
}

