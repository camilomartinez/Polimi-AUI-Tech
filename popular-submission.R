# For aggregation
library(plyr)
# For evaluation
library("Metrics")
# Custom common functions
source("C:/Polimi/aui-rs/Polimi-AUI-Tech/custom-functions.R")

# Path were data is located
setwd("C:/Polimi/aui-rs/Polimi-AUI-Tech/data")

# Complete training user rating matrix
URM <- read.csv("train.csv")

mostPopularMovies <- mostPopularItems(URM)

#Wheter we are evaluating or submitting results
evaluation = TRUE

# Test users Id
testUsers <- read.csv("test.csv")
# Get ratings from test users
testRatings <- merge(testUsers, URM)
# Ratings without test users
nonTestRatings <- URM[! URM$UserId %in% testUsers$UserId,]
if (evaluation) {
  knownRatings = nonTestRatings
} else {
  knownRatings = testRatings
}
# Movies seen by each user
knownItems = ddply(knownRatings, "UserId", summarise, ItemIds = list(ItemId))
# Generate a recommendation per user
recommendations <- ddply(knownItems, "UserId", function(row) {
  if (evaluation)
  {
    # Most popular Movies
    data.frame(ItemId = mostPopularMovies[1:5])
  } else {
    # Retrieve seen movies Id
    seenMovies <- row$ItemIds[[1]]
    # Remove seen from popular
    notSeenPopularMovies <- setdiff(mostPopularMovies,seenMovies)
    # 5 most popular not seen
    data.frame(ItemId = notSeenPopularMovies[1:5])
  }
})
# Aggregating recommendations per user
recommendedPerUser <- ddply(recommendations, "UserId", summarise,
                           ItemIds = list(ItemId),
                           RecommendedMovieIds = paste(ItemId, collapse = " "))

if (evaluation) {
  #Evaluation with optimistic MAP
  getItemIdsVector <- function(df) {
    df$ItemIds[[1]]
  }
  actual = dlply(knownItems, "UserId", getItemIdsVector)
  predicted = dlply(recommendedPerUser, "UserId", getItemIdsVector)
  mapk(5,actual, predicted)  
} else {
  # submission in output format
  # Output format
  submission <- recommendedPerUser
  # Drop vector of Ids for the output
  submission$ItemIds <- NULL
  write.table(submission, 
              file="submission.csv", 
              sep=",", quote=FALSE, 
              row.names = FALSE)
}

