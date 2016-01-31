source("trainESSVM.R")
source("predictESSVM.R")
source("ret.R")
## Author: Lili Sun
## last updated: 2016-1-30

bruteforce_exp_ESSVM <- function(data, 
                           initLen = 100,
                           inSize = 5,
                           outSize = 1,
                           resSize = 100,
                           alpha = 0.8,
                           leakingRate = 0.7,
                           testidx = 20150000,
                           testend = 20150900,
                           tradetype = "all",
                           tradeloss = 0,
                           trainLog = F){
  
  ## Subset data
  data = data[data$V1 < testend,]
  
  ## Shift, and resign `n`
  n <- nrow(data)
  shift <- 1+inSize
  n <- n - shift
  
  idx <- sum(data$V1 < testidx)  - shift
  pred = rep(0, n-idx)
  
  if(idx < initLen){
    cat("Error: illegal index\n")
    return(list("ac"=NA, "profit"=NA))
  }
  
  ## i is the training length of the data 
  for(i in idx:(n-1)){
    ## train a model
    model <- trainESSVM(data = data$V6, trainLen = i, inSize = inSize, trainLog = F, 
                      leakingRate = leakingRate,
                      alpha = alpha,
                      resSize = resSize,
                      initLen = initLen)
    ## predict the next first
    pred[i-idx+1] <- predictESSVM(model = model, data = data$V6, trainLen = i, testLen = 1)
  }
  
  ## Remove the last predict
  pred = pred[1:(length(pred)-1)]
  
  ## Calculate True Change rate
  targetY <- tail(data$V6[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))
  ## Calculate test signal and true signal
  signal   <- unlist(lapply(t(pred),     function(x){ifelse(x == 0, 0, x/abs(x))}))
  signal.Y <- unlist(lapply(t(targetY), function(x){ifelse(x == 0, 0, x/abs(x))}))
  
  
  profit = rep(0, length(signal))
  for(i in 1:length(signal)){
    profit[i] = ret(signal[i], targetY[i], type = tradetype, tradeloss = tradeloss)
  }
  
  return(list("ac" = sum(profit >0) / length(profit) , "profit" = sum(profit), 'pred' = pred ))
}

