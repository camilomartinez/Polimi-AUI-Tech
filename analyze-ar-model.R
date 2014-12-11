library(recommenderlab)

source("modified-packages.R")
## register recommender
recommenderRegistry$delete_entry(method="AR", dataType = "realRatingMatrix")
recommenderRegistry$set_entry(
  method="AR", dataType = "realRatingMatrix", fun=REAL_AR,
  description="Recommender based on association rules.",
  parameters=.REAL_AR_param
)
source("custom-functions.R")

trainCsv = ReadCsvData("train")
urm <- as(trainCsv, "realRatingMatrix")
recommender <- Recommender(urm, method = "AR", param=list(measure="confidence", support=0.02, maxlen=2))
# Get association rules from model
rules <- (getModel(recommender)$rule_base)
# Display statistics
summary(rules)

inspect(head(rules, 50))
