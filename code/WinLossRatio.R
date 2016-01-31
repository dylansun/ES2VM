WinLossRatio <- function(R){
  
  method = c("WinTimes",
             "LossTimes",
             "AvgProfit",
             "AvgLoss",
             "WinLossRate",
             "WinRate"
  )
  
  colNames <- names(R)
  R = checkData(R, method = "matrix")
  
#   WinTimes    <- sum(R>0)
#   LossTimes   <- sum(R<0)
#   AvgProfit   <- 
#   AvgLoss     <- sum(R[R<0])/LossTimes
#   WinLossRate <- AvgProfit / AvgLoss
#   WinRate     <- WinTimes / (WinTimes + LossTimes)
  
  result <- matrix(0,nrow = length(method), ncol = ncol(R))
  for(i in 1:ncol(R)){
    result[1,i] <- sum(R[,i]>0)
    result[2,i] <- sum(R[,i]<0)
    result[3,i] <- sum(R[R[,i]>0, i])/result[1,i]
    result[4,i] <- sum(R[R[,i]<0, i])/result[2,i]
    result[5,i] <- abs(result[3,i] / result[4,i]) 
    result[6,i] <- result[1,i] / (result[1,i] + result[2,i])
  }
  
  colnames(result) <- colNames
  rownames(result) <- method
  return(result)
}