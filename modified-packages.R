library("Metrics")

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
              # Computing Mean Average Precision from Metrics Package
              # and properly appending to results
              predicted <- as(x,"list")
              n <- max(sapply(predicted, length))
              actual <- as(data,"list")
              map <- mapk(n, actual, predicted)
              # Add metric with name
              res <- append(res, map)
              names(res)[length(res)] <- "MAP"
            }       
            res
          })

myApk <- function(k, actual, predicted)
{
  score <- 0.0
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
