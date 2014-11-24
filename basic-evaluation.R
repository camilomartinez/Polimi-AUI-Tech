library(recommenderlab)

source("modified-packages.R")

#Preparing training and test data
trainCsv = read.csv("data/train.csv")
# Filtering out test ratings
testUserIds = read.csv("data/test.csv")
nonTestRatings <- trainCsv[! trainCsv$UserId %in% testUserIds$UserId,]

# Algorithms used for the evaluation
algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "association rules" = list(name="AR", param=list(support=0.02))
)

# Coerce to the class used by recommenderlab
urm <- as(nonTestRatings, "realRatingMatrix")
# Sample to speed up calculations
#urmSample <- sample(urm, 1000)
# Use all users to more accurate evaluations
urmSample <- urm
# Binarize just according to ratings
#urmBinary <- binarize(urmSample, minRating=1)

scheme <- evaluationScheme(urmSample,
                           # Speed up calculations only one pass
                           #method = "split",
                           #train = .9,
                           method = "cross-validation",
                           given = 5,
                           goodRating = 4) # Cold-start problem

# Exhaustive evaluate at each k
#evaluateN = c(1,2,3,4,5)
# Fast linear approx with k 1 and 5
evaluateN = c(1,5)
results <- evaluate(scheme, algorithms, n=evaluateN)

#Pay attention mainly to precision
plot (results, "prec/rec", annotate = FALSE, legend="bottomright")

formatString <- "MAP@5 for %s: %.4f"
algoNames <- names(algorithms)
for (i in 1:length(algorithms)) {
  map <- avg(results[[i]])[2,"MAP"]
  print(sprintf(formatString, algoNames[i], map))
}

