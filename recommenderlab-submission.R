setwd("C:/Polimi/aui-rs/Polimi-AUI-Tech/data")

train.original = read.csv("train.csv")

# Coerce to the class used by recommenderlab
train <- as(train.original, "realRatingMatrix")

recommender <- Recommender(train, method = "SVD")

recommendation <- predict(recommender, 4, data=train, n=5)
