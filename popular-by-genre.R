# Custom common functions
source("custom-functions.R")
# Configurations constants
source("common-setup.R")

# Complete training user rating matrix
urm <- LoadURM()
urm <- urm[100000:108000,]

# Recommendation functions
recommend <- PopularByGenreRecommender(urm, kRemoveSeen, kNumberOfRecommendations)

# Retrieve items seen by the users
seenItems <- FilterSeenItems(urm, evaluation)

# Generate a recommendation per user
recommendedPerUser <- GenerateRecommendations(seenItems, recommend ,kSubmission)

# Either prints the MAP for training or write the submission
GenerateOutput(urm, recommendedPerUser, evaluation)
