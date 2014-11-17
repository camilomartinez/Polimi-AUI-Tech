# For aggregation
library(plyr)

# Custom common functions
source("custom-functions.R")

urm <- LoadURM()
goodRatings <- FilterGoodRatings(urm)
users <- LoadUsers()
ratingsWithUserMetadata <- merge(goodRatings, users)
mostPopularM <- MostPopularItems(ratingsWithUserMetadata[ratingsWithUserMetadata$Gender == "M",])
mostPopularF <- MostPopularItems(ratingsWithUserMetadata[ratingsWithUserMetadata$Gender == "F",])
