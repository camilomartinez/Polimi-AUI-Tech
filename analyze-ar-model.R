library(recommenderlab)

source("modified-packages.R")
source("custom-functions.R")

trainCsv = ReadCsvData("train")
urm <- as(trainCsv, "realRatingMatrix")
recommender <- Recommender(urm, method = "AR", param=list(measure="hyperConfidence", maxlen=2))
# Get association rules from model
rules <- (getModel(recommender)$rule_base)
# Display statistics
summary(rules)

inspect(head(rules, 50))
