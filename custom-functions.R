# For count
library(plyr)

# Returns a functions that takes user row input as parameter
# removeSeen: Wheter to recommend already seen interactions
# n: how many items to recommend
PopularRecommender <- function(urm, removeSeen, n) {
  mostPopularItems <- MostPopularItems(urm)
  function(userRow) {
    if (removeSeen) {
      recommendedItems <- RemoveSeenItems(userRow, mostPopularItems)
      # Take as many as n items
      head(recommendedItems, n)
    } else {
      # Take as many as n items
      head(mostPopularItems, n)
    }
  }
}

# Return most popular items by count of goodRatings
# Expects a User Rating Matrix data frame
# with the columns "UserId", "ItemId" and "Rating"
MostPopularItems <- function(URM) {
  # Only consider ratings >= 4
  goodRatings <- FilterGoodRatings(URM,4)
  # Ordering movies by popularity
  mostPopularMovies <- OrderItemsByCount(goodRatings)
}

FilterGoodRatings <- function(URM, threshold) {
  goodRatings <- URM[URM$Rating >= threshold,]
  row.names(goodRatings) <- NULL
  return(goodRatings)
}

# Expects a dataFrame with "ItemId" repeated
# Returns a vector ordered by count
OrderItemsByCount <- function(df) {
  # Ordering items by count
  itemCount <- count(df, "ItemId")
  itemsByCount <- arrange(itemCount, desc(freq))
  # Only items as a vector
  return(as.vector(itemsByCount[["ItemId"]]))
}

# First parameter should have an ItemIds column with a singleton list with a vector
# Second should be the recommendations available to this user as a vector of item ids
RemoveSeenItems <- function(userRow, recommendations)
{
  seenItems <- userRow$ItemIds[[1]]
  # Remove from recommendations
  setdiff(recommendations, seenItems)
}



# Expect the user ratings matrix and returns the
# most popular movies for each genre
mostPopularByGenre <- function(urm, movies) {
  goodRatings <- filterGoodRatings(urm,4)
  # Expand movies by genre with a row per movie and gender
  moviesByGenre <- ddply(movies, "MovieId", function(row) {
    # Extract genre vector
    Genre <- row$Genre[[1]]
    data.frame(Genre)
  })
  # Anotate ratings with genre
  ratingsWithGenre <- merge(goodRatings, moviesByGenre, by.x="ItemId", by.y="MovieId")
  # Order Items by popularity for each Genre
  itemPopularityByGenre <- ddply(ratingsWithGenre, "Genre", function(df) {
    itemCount <- orderItemsByCount(df)
    data.frame(ItemId = itemCount)
  })
  # Aggregate into genre and vector
  popularByGenre <- summariseItemsBy(itemPopularityByGenre, "Genre")
}

ItemsSeenByTestUsers <- function(urm) {
  testUserIds <- LoadTestUserIds()
  testRatings <- merge(testUserIds, urm)
  summariseItemsBy(testRatings, "UserId")
}

ItemsSeenByNonTestUsers <- function(urm) {
  testUserIds <- LoadTestUserIds()
  nonTestRatings <- urm[! urm$UserId %in% testUserIds$UserId,]
  summariseItemsBy(nonTestRatings, "UserId")
}

LoadTestUserIds <- function() {
  ReadCsvData("test")
}

# Aggregate items as a vector grouping by a column
summariseItemsBy <- function(df, groupBy) {
  ddply(df, groupBy, summarise, ItemIds = list(ItemId))
}

LoadURM <- function() {
  # Get movie metadata
  ReadCsvData("train")
}

LoadMovies <- function() {
  # Get movie metadata
  ReadCsvData("movieMetaCorrected")
  # Split genre string by '|' grouping by MovieId
  splitGenre = 
  movies$Genre <- by(movies, movies$MovieId,FUN=function(row) {
    strsplit(as.character(row$Genre), '|', fixed=TRUE)
  })
  return(movies)
}

ReadCsvData <- function(filename) {
  #Deafult separator is space
  completeFilename <- paste("data/",filename,".csv",sep='')
  read.csv(completeFilename)
}

# Function used to generate the recommendation data frame
# seenItems: Items already seen by the user
# recommendationFunction: receiving a user row
# submission: boolean to indicated wheter to prepare for submission or evaluation
GenerateRecommendations <- function(seenItems, recommendationFunction, submission) {
  recommendations <- ddply(seenItems, "UserId", function(row) {
    ItemId <- recommend(row)
    data.frame(ItemId)
  })
  # Aggregating recommendations per user
  # As a space separated string for submission
  if(submission)
  {
    ddply(recommendations, "UserId", summarise,
          RecommendedMovieIds = paste(ItemId, collapse = " "))
  } else {
    ddply(recommendations, "UserId", summarise,
          ItemIds = list(ItemId))
  }
}

# df is a data frame with a "ItemIds" Column as a list with a single vector element
GetItemIdsVector <- function(df) {
  df$ItemIds[[1]]
}