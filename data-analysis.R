setwd("C:/Polimi/aui-rs/Polimi-AUI-Tech/data")


train = read.csv("train.csv")

# Coerce to the class used by recommenderlab
r <- as(train, "realRatingMatrix")

# Distribution of ratings
hist(getRatings(r), breaks=6)
# Row centering
hist(getRatings(normalize(r)), breaks=100)
# Z-score per row
hist(getRatings(normalize(r, method="Z-score")), breaks=100)
# Items rated per user
items.rated.per.user = rowCounts(r)
data = items.rated.per.user
hist(data, breaks = 50)
median(data)
# Mean rating per item
mean.rating.per.item = colMeans(r)
data = mean.rating.per.item
hist(data, breaks = 20)
mean(data)



