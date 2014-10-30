setwd("C:/Polimi/aui-rs/Polimi-AUI-Tech/data")


train.original = read.csv("shortTrain.csv")

# Coerce to the class used by recommenderlab
train.sparse <- as(train.original, "realRatingMatrix")

scheme <- evaluationScheme(train.sparse,
                           method = "split",
                           train = .9,
                           #method = "cross-validation",
                           given = 5,  # This match with the 5 ratings
                                       # cold start problem
                           goodRating = 4) # rating over which items
                                           # are relevant

algorithms <- list(
  "random" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=50, minRating=3)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

results <- evaluate(scheme, algorithms, n=c(1,3,5,10))

plot (results, annotate = 1:4, legend="topleft")
plot (results, "prec/rec", annotate = 3)
