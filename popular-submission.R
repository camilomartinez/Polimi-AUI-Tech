# Custom common functions
source("custom-functions.R")
# Configurations constants
source("common-setup.R")

# Complete training user rating matrix
urm <- LoadURM()

# Recommendation functions
recommend <- PopularRecommender(urm, kRemoveSeen, kNumberOfRecommendations)

# Retrieve items seen by the users
seenItems <- FilterSeenItems(urm, evaluation)

# Generate a recommendation per user
recommendedPerUser <- GenerateRecommendations(seenItems, recommend ,kSubmission)

# Either prints the MAP for training or write the submission
GenerateOutput(seenItems, recommendedPerUser, evaluation)
