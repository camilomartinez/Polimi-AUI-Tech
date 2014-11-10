# For aggregation
require(data.table)
require(plyr)

# Path were data is located
setwd("C:/Polimi/aui-rs/Polimi-AUI-Tech/data")

# Complete training user rating matrix
URM <- read.csv("train.csv")

# Only consider ratings >= 4
goodRatings <- URM[URM$Rating >= 4,]

# Ordering movies by popularity
itemCount <- count(goodRatings, "ItemId")
moviesByPopularity <- arrange(itemCount, desc(freq))
tenMostPopularMovies <- as.vector(moviesByPopularity[1:10,"ItemId"])

# Generate recommendations by removing known movies per user
testUsers <- read.csv("test.csv")
# Get ratings from test users
testRatings <- merge(testUsers, URM)
# Movies seen by each testUser
knownItems = ddply(testRatings, "UserId", summarise, ItemIds = list(ItemId))
# Generate a recommendation per user
submission <- ddply(knownItems, "UserId", function(row) {
  # Retrieve seen movies Id
  seenMovies <- row$ItemIds[[1]]
  # Remove seen from popular
  notSeenPopularMovies <- setdiff(tenMostPopularMovies,seenMovies)
  # 5 most popular not seen are concatenated separated by space
  RecommendedMovieIds <-paste(notSeenPopularMovies[1:5], collapse=" ")
  data.frame(RecommendedMovieIds)
})

write.table(submission, file="submission.csv", sep=",", quote=FALSE, row.names = FALSE)

users = read.csv("userMeta.csv")
movies = read.csv("movieMetaCorrected.csv")
