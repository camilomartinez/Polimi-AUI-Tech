# For aggregation
library(plyr)
# For evaluation
library("Metrics")

# Returns a functions that takes user row input as parameter
# removeSeen: Wheter to recommend already seen interactions
# numberOfRecommendations: how many items to recommend
PopularRecommender <- function(urm, removeSeen, numberOfRecommendations) {
  mostPopularItems <- MostPopularItems(urm)
  function(userRow) {
    RecommendForUser(userRow, mostPopularItems, removeSeen, numberOfRecommendations)
  }
}

# Returns a functions that takes user row input as parameter
# removeSeen: Wheter to recommend already seen interactions
# movies metadata
# numberOfRecommendations: how many items to recommend
PopularByGenreRecommender <- function(urm, removeSeen, numberOfRecommendations) {
  movies <- LoadMovies()
  mostPopularByGenre <- MostPopularByGenre(urm, movies)
  moviesWithGenre <- ddply(movies, "MovieId", function(row) {
    # Extract genre vector
    Genre <- row$Genre[[1]]
    data.frame(Genre)
  })
  function(userRow) {
    userRatings <- urm[urm$UserId == userRow$UserId,]
    # Could be filtered by 3 here to be more flexible
    goodRatings <- FilterGoodRatings(userRatings, 3)
    # Anotate ratings with genre
    ratingsWithGenre <- merge(goodRatings, moviesWithGenre, by.x="ItemId", by.y="MovieId")
    itemCountPerGenre <- count(ratingsWithGenre, "Genre")
    itemCountPerGenre <- arrange(itemCountPerGenre, desc(freq))
    totalCount <- sum(itemCountPerGenre$freq)
    itemCountPerGenre$share <- ceiling(5 * itemCountPerGenre$freq / totalCount)
    moviesTaken <- ddply(itemCountPerGenre, "Genre", function(genreRow) {
      genre <- genreRow$Genre
      ItemId <- head(GetItemIdsVector(mostPopularByGenre[mostPopularByGenre$Genre == genre,]),genreRow$share)
      data.frame(ItemId)
    })
    # Extract recommended items
    recommendedItems <- as.vector(moviesTaken$ItemId)
    # TODO: For each genre where the share is greater than 0 take some movies
    #GetItemIdsVector(mostPopularByGenre[mostPopularByGenre$Genre == "Drama",])
    RecommendForUser(userRow, recommendedItems, removeSeen, numberOfRecommendations)
  }
}

# Recommend n items removing those seen if indicated 
RecommendForUser <- function(userRow, recommendedItems, removeSeen, numberOfRecommendations) {
  if (removeSeen) {
    itemsToRecommend <- RemoveSeenItems(userRow, recommendedItems)
  } else {
    itemsToRecommend <- recommendedItems
  }
  # Take as many as recommendations as requested
  head(recommendedItems, numberOfRecommendations)
}

# Return most popular items by count of goodRatings
# Expects a User Rating Matrix data frame
# with the columns "UserId", "ItemId" and "Rating"
MostPopularItems <- function(urm) {
  goodRatings <- FilterGoodRatings(urm)
  # Ordering movies by popularity
  mostPopularMovies <- OrderItemsByCount(goodRatings)
}

# As per competition forum
FilterGoodRatings <- function(urm, threshold = 4) {
  goodRatings <- urm[urm$Rating >= threshold,]
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
  seenItems <- GetItemIdsVector(userRow)
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
  popularByGenre <- SummariseItemsBy(itemPopularityByGenre, "Genre")
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
  SummariseItemsBy(testRatings, "UserId")
}

ItemsSeenByNonTestUsers <- function(urm) {
  testUserIds <- LoadTestUserIds()
  nonTestRatings <- urm[! urm$UserId %in% testUserIds$UserId,]
  SummariseItemsBy(nonTestRatings, "UserId")
}

LoadTestUserIds <- function() {
  ReadCsvData("test")
}

# Aggregate items as a vector grouping by a column
SummariseItemsBy <- function(df, groupBy) {
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
    if (length(ItemId) == 0)
    {
      ItemId = c(1)
    }
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
GetItemIdsVector <- function(userRow) {
  userRow$ItemIds[[1]]
}

# print map or generate submission file
GenerateOutput <- function(urm, recommendedPerUser, forEvaluation) {
  if(forEvaluation) {
    CalculateMap(urm, recommendedPerUser)
  } else {
    WriteSubmission(recommendedPerUser)
  }
}

# Print the MAP at the indicated number of recommendations
CalculateMap <- function(urm, recommendedPerUser) {
  # Filter relevant items per user
  goodRatings <- FilterGoodRatings(urm)
  relevantItems <- ItemsSeenByNonTestUsers(goodRatings)
  relevantItemsPerUser = dlply(relevantItems, "UserId", GetItemIdsVector)
  # Aggregate predictions per user
  predictedPerUser = dlply(recommendedPerUser, "UserId", GetItemIdsVector)
  # Compute number of recommendations done
  samplerecommendation = GetItemIdsVector(recommendedPerUser[1,])
  numberOfRecommendations = length(samplerecommendation)
  # Mean average precision from metrics package
  mapk(numberOfRecommendations,relevantItemsPerUser, predictedPerUser)
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