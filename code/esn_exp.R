source("trainESN.R")
source("predictESN.R")
source("ret.R")
source("WinLossRatio.R")

esn_exp <- function( data, inSize = 5, resSize = 100, alpha = 0.8, leakingRate = 0.7 ){
n <- nrow(data)
shift <- 1+inSize
n <- n - shift
idx.2013 <- sum(data$V1 < 20130000)  - shift 
idx.2014 <- sum(data$V1 < 20140000)  - shift
idx.2015 <- sum(data$V1 < 20150000)  - shift
pred = rep(0, n-idx.2013)

## i is the training length of the data 
for(i in idx.2013:(n-1)){
  ## train a model
  model <- trainESN(data = data$V6, trainLen = i, inSize = inSize, 
                    trainLog = F ,alpha = alpha, leakingRate = leakingRate,
                    resSize = resSize)
  ## predict the next first
  pred[i-idx.2013+1] <- predictESN(model = model, data = data$V6, trainLen = i, testLen = 1)
}

## Calculate True Change rate
targetY <- tail( data$V6[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] - 1, n=length(pred))
## Calculate test signal and true signal
signal   <- unlist(lapply(t(pred),     function(x){ifelse(x == 0, 0, x/abs(x))}))
signal.Y <- unlist(lapply(t(targetY), function(x){ifelse(x == 0, 0, x/abs(x))}))

# ## Evaluate the test mse and ac
# mse = sum((targetY-pred)^2) / length(targetY)
# print(paste("Testing MSE = ", mse))
# ac  = sum(signal == signal.Y) / length(signal.Y)
# print(paste("Testing AC = ", ac))

tradetype = "pos"

profit = rep(0, length(signal))
for(i in 1:length(signal)){
  profit[i] = ret(signal[i], targetY[i], type = tradetype, tradeloss = 3/10000)
}


t.pred        <- tail(data$V1, n = length(pred))
date          <- as.Date(as.character(t.pred), format="%Y%m%d")
profit.xts    <- xts(profit, order.by = date)
targetY.xts   <- xts(targetY, order.by = date)


## report required metrics
# WinLossRatio(profit.xts)
# print(paste("Max Drawdown = ", maxDrawdown(profit.xts)))
# print(paste("Sharpe Ratio = ", SharpeRatio.annualized(profit.xts)))
# print(paste("Final Return = ", Return.cumulative(profit.xts, geometric = F)))

return(list(ac = WinLossRatio(profit.xts)[6], ret = Return.cumulative(profit.xts, geometric = F)))
}
