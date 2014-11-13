# For aggregation
library(plyr)
# For evaluation
library("Metrics")

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

# Returns a functions that takes user row input as parameter
# removeSeen: Wheter to recommend already seen interactions
# movies metadata
# n: how many items to recommend
PopularByGenreRecommender <- function(urm, movies, removeSeen, n) {
  mostPopularByGenre <- MostPopularByGenre(urm, movies)
  moviesByGenre <- ddply(movies, "MovieId", function(row) {
    # Extract genre vector
    Genre <- row$Genre[[1]]
    data.frame(Genre)
  })
  function(userRow) {
    # Anotate ratings with genre
    goodRatings <- FilterGoodRatings(userRow)
    ratingsWithGenre <- merge(goodRatings, moviesByGenre, by.x="ItemId", by.y="MovieId")
    itemsPerGenre <- ddply(ratingsWithGenre, "Genre", CountItems(df))
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
MostPopularItems <- function(urm) {
  goodRatings <- FilterGoodRatings(urm)
  # Ordering movies by popularity
  mostPopularMovies <- OrderItemsByCount(goodRatings)
}

FilterGoodRatings <- function(urm) {
  # As per competition forum
  kThreshold = 4
  goodRatings <- urm[urm$Rating >= kThreshold,]
  row.names(goodRatings) <- NULL
  return(goodRatings)
}

# Expects a dataFrame with "ItemId" repeated
# Returns a vector ordered by count
OrderItemsByCount <- function(df) {
  # Ordering items by count
  itemCount <- CountItems(df)
  itemsByCount <- arrange(itemCount, desc(freq))
  # Only items as a vector
  return(as.vector(itemsByCount[["ItemId"]]))
}

CountItems <- function(df) {
  count(df, "ItemId")
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
MostPopularByGenre <- function(urm, movies) {
  goodRatings <- FilterGoodRatings(urm)
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
    itemCount <- OrderItemsByCount(df)
    data.frame(ItemId = itemCount)
  })
  # Aggregate into genre and vector
  popularByGenre <- summariseItemsBy(itemPopularityByGenre, "Genre")
}

# Seen items for evaluation or not
FilterSeenItems <- function(urm, forEvaluation) {
  if (evaluation) {
    seenItems = ItemsSeenByNonTestUsers(urm)
  } else {
    seenItems = ItemsSeenByTestUsers(urm)
  }
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
  movies <- ReadCsvData("movieMetaCorrected")
  # Split genre string by '|' grouping by MovieId
  movies$Genre <- by(movies, movies$MovieId, FUN=function(row) {
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
GetItemIdsVector <- function(row) {
  row$ItemIds[[1]]
}

# print map or generate submission file
GenerateOutput <- function(seenItems, recommendedPerUser, forEvaluation) {
  if(forEvaluation) {
    CalculateMap(seenItems, recommendedPerUser)
  } else {
    WriteSubmission(recommendedPerUser)
  }
}

# Print the MAP at the indicated number of recommendations
CalculateMap <- function(seenItems, recommendedPerUser) {
  samplerecommendation = GetItemIdsVector(recommendedPerUser[1,])
  numberOfRecommendations = length(samplerecommendation)
  # TODO: filter actual relevant items
  relevantItems <- seenItems
  actual = dlply(relevantItems, "UserId", GetItemIdsVector)
  predicted = dlply(recommendedPerUser, "UserId", GetItemIdsVector)
  # Mean average precision from metrics package
  mapk(numberOfRecommendations,actual, predicted)
}

WriteSubmission <- function(recommendedPerUser) {
  # submission in output format
  submission <- recommendedPerUser
  # Drop vector of Ids for the output
  submission$ItemIds <- NULL
  write.table(submission, 
              file="submissions/submission.csv", 
              sep=",", quote=FALSE, row.names=FALSE)
}