# Load required library
library(recommenderlab) # package being evaluated
library(ggplot2) # For plots

# Load the data we are going to work with
data(MovieLense)

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

scheme <- evaluationScheme(MovieLense, method = "split", train = .9, k = 1, given = 10, goodRating = 4)

algorithms <- list( "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),"popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),"user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=50, minRating=3)),"item-based CF" = list(name="IBCF", param=list(normalize = "Z-score")))

results <- evaluate(scheme, algorithms, n=c(1,3,5,10,15,20))

plot (results, annotate = 1:4, legend="topleft")
plot (results, "prec/rec", annotate = 3)
