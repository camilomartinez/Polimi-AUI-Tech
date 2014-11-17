library(recommenderlab)

source("modified-recommender-lab.R")

#Preparing training and test data
trainCsv = read.csv("data/train.csv")
# Filtering out test ratings
testUserIds = read.csv("data/test.csv")
nonTestRatings <- trainCsv[! trainCsv$UserId %in% testUserIds$UserId,]

# Algorithms used for the evaluation
algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "association rules" = list(name="AR")
)

# Coerce to the class used by recommenderlab
urm <- as(nonTestRatings, "realRatingMatrix")
# Sample to speed up calculations
urmSample <- sample(urm, 1000)
# Use all users to more accurate evaluations
#urmSample <- urm
# Binarize just according to ratings
urmBinary <- binarize(urmSample, minRating=1)

scheme <- evaluationScheme(urmBinary,
                           # Speed up calculations only one pass
                           #method = "split",
                           #train = .9,
                           method = "cross-validation",
                           given = 5) # Cold-start problem

results <- evaluate(scheme, algorithms, n=c(1,2,3,4,5))

#Pay attention mainly to precision
plot (results, "prec/rec", annotate = 1:2, legend="bottomright")

map5pop <- avg(results[[1]])[5,"MAP"]
map5ar <- avg(results[[2]])[5,"MAP"]
formatString <- "MAP@5 for %s: %.4f"
sprintf(formatString, "popular", map5pop)
sprintf(formatString, "AR", map5ar)

