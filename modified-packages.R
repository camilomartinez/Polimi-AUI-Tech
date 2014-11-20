############### recommender lab package ####################

library(recommenderlab)

# Adding mean average precision metric
setMethod("calcPredictionAccuracy", signature(x= "topNList", 
                                              data = "binaryRatingMatrix"),
          
          function(x, data, byUser=FALSE, given=NULL, ...) {
            if(is.null(given)) stop("You need to specify how many items were given for the prediction!")
            
            TP <- rowSums(as(x, "ngCMatrix") * as(data, "ngCMatrix"))
            TP_FN <- rowCounts(data)
            TP_FP <- rowCounts(x)
            FP <- TP_FP - TP
            FN <- TP_FN - TP
            TN <-  ncol(data) - given - TP - FP - FN
            
            ## calculate some important measures
            precision <- TP / (TP + FP) 
            recall <- TP / (TP + FN) 
            TPR <- recall 
            FPR <- FP / (FP + TN)
            
            res <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)
            
            if(!byUser) {
              res <- colMeans(res, na.rm=TRUE)
              ########## Start Modified ############
              # Computing Mean Average Precision from Metrics Package
              # and properly appending to results
              predicted <- as(x,"list")
              n <- max(sapply(predicted, length))
              actual <- as(data,"list")
              map <- mapk(n, actual, predicted)
              # Add metric with name
              res <- append(res, map)
              names(res)[length(res)] <- "MAP"
              ########## End Modified ##############
            }       
            res
          })

## recommendations based on association rules
# Modified to binarize before recommendations are made

source("RECOM_BIN_AR.R")

.REAL_AR_param <- list(
  # new parameter to control the minimum rating to binarize
  minRating = 1
)
# Also use binary association rules parameters
.REAL_AR_param <- append(.REAL_AR_param, .BIN_AR_param)

REAL_AR <- function(data, parameter = NULL) {
  
  ## parameters
  p <- .get_parameters(.REAL_AR_param, parameter)
  
  binData <- binarize(data, minRating=p$minRating)
  
  # Drop non BIN_AR parameters
  pBin <- p
  pBin$minRating <- NULL
  # Wrap binary association rules recommender
  binRecommender <- Recommender(binData, method = "AR", param=pBin)
  binModel <- getModel(binRecommender)
  
  model <- c(list(
      description = "AR: rule base",
      rule_base = binModel$rule_base
    ), p 
  ) 
  
  internalPredict <- function(model, newdata, n=10, data=NULL, ...) {
    
    ## newdata are userid
    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }
    # Binarize data before passing it to the wrapped recommender
    newBinData <- binarize(newdata, minRating=p$minRating)
    predict(binRecommender, newBinData, n)
  }
  
  ## construct recommender object
  new("Recommender", method = "AR", dataType = "realRatingMatrix",
      ntrain = nrow(data), model = model, predict = internalPredict)
}

## register recommender
recommenderRegistry$delete_entry(method="AR", dataType = "realRatingMatrix")
recommenderRegistry$set_entry(
  method="AR", dataType = "realRatingMatrix", fun=REAL_AR,
  description="Recommender based on association rules.",
  parameters=.REAL_AR_param
)

############### Metrics package ####################

library("Metrics")

myApk <- function(k, actual, predicted)
{
  score <- 0.0
  ###### Modified ############
  # Added line to control length of the prediction
  # If empty then 0
  if ((length(predicted) > 0) && (k > 0))
  {
    cnt <- 0.0
    for (i in 1:min(k,length(predicted)))
    {
      if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
      {
        cnt <- cnt + 1
        score <- score + cnt/i 
      }
    }
    score <- score / min(length(actual), k)
  }
  score
}
assignInNamespace("apk", myApk, ns="Metrics")
