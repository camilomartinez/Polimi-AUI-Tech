library(recommenderlab)

source("modified-recommender-lab.R")

trainCsv = read.csv("data/train.csv")
# Filtering out test ratings
testUserIds = read.csv("data/test.csv")
nonTestRatings <- trainCsv[! trainCsv$UserId %in% testUserIds$UserId,]

# Coerce to the class used by recommenderlab
urm <- as(nonTestRatings, "realRatingMatrix")
urmSample <- sample(urm, 100)
urmBinary <- binarize(urmSample, minRating=1)

scheme <- evaluationScheme(urmBinary,
                           #method = "split",
                           #train = .9,
                           method = "cross-validation",
                           given = 5)#,  # This match with the 5 ratings
# cold start problem
#goodRating = 4) # rating over which items
# are relevant

algorithms <- list(
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "association rules" = list(name="AR")
)

results <- evaluate(scheme, algorithms, n=c(1,2,3,4,5))

#Pay attention mainly to precision
plot (results, "prec/rec", annotate = 1:2, legend="bottomright")

prec5pop <- avg(results[[1]])[5,"precision"]
prec5ar <- avg(results[[2]])[5,"precision"]
formatString <- "Precision@5 for %s: %.4f"
sprintf(formatString, "popular", prec5pop)
sprintf(formatString, "AR", prec5ar)

