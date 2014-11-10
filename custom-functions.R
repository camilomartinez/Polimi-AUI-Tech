# For count
library(plyr)

# Return most popular items by count of goodRatings
# Expects a User Rating Matrix data frame
# with the columns "UserId", "ItemId" and "Rating"
mostPopularItems <- function(URM) {
  # Only consider ratings >= 4
  goodRatings <- URM[URM$Rating >= 4,]
  # Ordering movies by popularity
  itemCount <- count(goodRatings, "ItemId")
  itemsByPopularity <- arrange(itemCount, desc(freq))
  # Only output movies as a vector
  mostPopularMovies <- as.vector(itemsByPopularity[["ItemId"]])
}

loadMovies <- function() {
  # Get movie metadata
  movies <- read.csv("movieMetaCorrected.csv")
  # Split genre string by '|' grouping by MovieId
  splitGenre = function(row) {
    strsplit(as.character(row$Genre), '|', fixed=TRUE)
  }
  movies$Genre <- by(movies, movies$MovieId,FUN=splitGenre)
  return(movies)
}