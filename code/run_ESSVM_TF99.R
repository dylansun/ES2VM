## Author: Lili Sun
##
## updated: 01-30-2016
##          simplify code
## updated: 01-25-2016
##          add comments
## About this script: run ESN on TF99 with slide window data

## Load other utility functions
source("trainESSVM.R")
source("predictESSVM.R")
source("ret.R")
source("WinLossRatio.R")
source("stoploss.R")

## load data preprocess  
data <- read.table("../data//TF99.txt")
testidx <- 20150900
testend <- 20160100
trainLog = F 
leakingRate = 0.2
alpha =0.2
resSize = 70
initLen = 100

data <- data[data$V1 < testend,]

n <- nrow(data)
inSize <- 5
shift <- 1+inSize
n <- n - shift

idx <- sum(data$V1 < testidx)  - shift
pred = rep(0, n-idx)

## train and predict TF99 with slide window data 
## i is the training length of the data 
for(i in idx:(n-1)){
  ## train a model
  model <- trainESSVM(data = data$V6, trainLen = i, inSize = inSize, trainLog = trainLog, 
                      leakingRate = leakingRate, 
                      alpha = alpha, 
                      resSize = resSize,
                      initLen = initLen)
  ## predict the next first
  pred[i-idx+1] <- predictESSVM(model = model, data = data$V6, trainLen = i, testLen = 1)
}

## Remove the last predict
## The last signal doesn't have real label 
pred = pred[1:(length(pred)-1)]

## Calculate True Change rate
targetY <- tail(data$V6[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))
## Calculate test signal and true signal
signal  <- unlist(lapply(t(pred),     function(x){ifelse(x == 0, 0, x/abs(x))}))
signal.Y <- unlist(lapply(t(targetY), function(x){ifelse(x == 0, 0, x/abs(x))}))

## Evaluate the test mse and ac
mse = sum((targetY-pred)^2) / length(targetY)
print(paste("Testing MSE = ", mse))
ac  = sum(signal == signal.Y) / length(signal.Y)
print(paste("Testing AC = ", ac))

tradetype = "all"
profit = rep(0, length(signal))
for(i in 1:length(signal)){
  profit[i] = ret(signal[i], targetY[i], type = tradetype, tradeloss = 0)
}

## add stop loss to refine profit
## profit:   alogrithm profit
## target.o: open price 
## target.h: high price
## target.l: low  price
## signal:   predict signal
## tradetype: 'pos' 
##            'all' 
##            'neg'
## pos.stoploss.threshold:
## neg.stoploss.threshold: 

target.o <- tail(data$V3[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))
target.h <- tail(data$V4[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))
target.l <- tail(data$V5[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))

pos.stoploss.threshold <- 0.001
neg.stoploss.threshold <- 0.001

profit <- stoploss(profit, target.o, target.h, target.l,signal, 
                   pos.stoploss.threshold, neg.stoploss.threshold, tradetype = 'all')

##
t.pred <- tail(data$V1, n = length(pred))
date <- as.Date(as.character(t.pred), format="%Y%m%d")
profit.xts    <- xts(profit, order.by = date)
targetY.xts  <- xts(targetY, order.by = date)

## report required metrics
WinLossRatio(profit.xts)
print(paste("Max Drawdown = ", maxDrawdown(profit.xts)))
print(paste("Sharpe Ratio = ", SharpeRatio.annualized(profit.xts)))
print(paste("Final Return = ", Return.cumulative(profit.xts, geometric = F)))


## Evaluate By year 
## Using the apply.yearly function in PerformanceAnalytics
se.xts = xts((targetY-pred)^2, order.by = date)
apply.yearly(se.xts, mean)
ac.xts  = xts(signal == signal.Y, order.by = date)
apply.yearly(ac.xts, mean)

apply.monthly(profit.xts, WinLossRatio)
apply.monthly(profit.xts, maxDrawdown)
apply.monthly(profit.xts, SharpeRatio.annualized)
apply.monthly(profit.xts, Return.cumulative, geometric = F)



## merge the TF99 real trend with the proposed profit time series
## for a comparative vision purpose.

merge.xts = merge.xts(profit.xts, targetY.xts);

## calculate cumulative sum of the target time series
## and the profit time series for adaptive y limitations
cumsum_merge = cumsum(merge.xts);
ylim_max = max(cumsum_merge);
ylim_min = min(cumsum_merge);

chart.CumReturns(merge.xts,geometric = F, ylim = c(ylim_min, ylim_max), type = "l", col =c("blue", "red"),
                 main = "Cumulative Return Plots of ES2VM and TF99"
)

legend("topleft",bty = "n", ncol = 1, cex = 1,  legend = c("ES2VM", "TF99"), col=c("blue","red"), lwd = 2)

