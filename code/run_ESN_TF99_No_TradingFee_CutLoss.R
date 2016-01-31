source("trainESN.R")
source("predictESN.R")
source("ret.R")
source("WinLossRatio.R")
## preprocess 
data <- read.table("../data//TF99.txt")

n <- nrow(data)
inSize <- 5
shift <- 1+inSize
n <- n - shift

idx.2015 <- sum(data$V1 < 20150000)  - shift
pred = rep(0, n-idx.2015)

## i is the training length of the data 
for(i in idx.2015:(n-1)){
  ## train a model
  model <- trainESN(data = data$V6, trainLen = i, inSize = inSize, trainLog = F)
  ## predict the next first
  pred[i-idx.2015+1] <- predictESN(model = model, data = data$V6, trainLen = i, testLen = 1)
}

## Remove the last predict
pred = pred[1:(length(pred)-1)]

save(pred, file = "../result/slideWinPred_TF99.RData")

## Calculate True Change rate
targetY <- tail(data$V6[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)] -1, n=length(pred))
## Calculate test signal and true signal
signal  <- unlist(lapply(t(pred),     function(x){ifelse(x == 0, 0, x/abs(x))}))
signal.Y <- unlist(lapply(t(targetY), function(x){ifelse(x == 0, 0, x/abs(x))}))

## Evaluate the test mse and ac
#mse = sum((targetY-pred)^2) / length(targetY)
#print(paste("Testing MSE = ", mse))
#ac  = sum(signal == signal.Y) / length(signal.Y)
#print(paste("Testing AC = ", ac))

tradetype = "all"

profit = rep(0, length(signal))
for(i in 1:length(signal)){
  profit[i] = ret(signal[i], targetY[i], type = tradetype, tradeloss = 0)
}


t.pred <- tail(data$V1, n = length(pred))
date <- as.Date(as.character(t.pred), format="%Y%m%d")
profit.xts    <- xts(profit, order.by = date)
targetY.xts  <- xts(targetY, order.by = date)

profit.xts = profit.xts["2015-09/"]
targetY.xts = targetY.xts["2015-09/"]

## report required metrics
WinLossRatio(profit.xts)
print(paste("Max Drawdown = ", maxDrawdown(profit.xts)))
print(paste("Sharpe Ratio = ", SharpeRatio.annualized(profit.xts)))
print(paste("Final Return = ", Return.cumulative(profit.xts, geometric = F)))

chart.CumReturns(profit.xts,geometric = F, ylim = c(-0.2, 1), type = "l", 
                 col = "blue", main = "Cumulative Return Plots of ESN and TF99"
)

lines(cumsum(targetY), type = "l", col = "red")
legend("topleft",bty = "n", ncol = 2, cex = 0.5,  legend = c("Standard ESN", "TF99"), col=c("blue","red"), lwd = 2)

targetY <- tail((data$V6[2:length(data$V6)] / data$V6[1:(length(data$V6)-1)]), n=length(pred))


## Evaluate By month 
## Using the apply.monthly function in PerformanceAnalytics

ac.xts  = xts(signal == signal.Y, order.by = date)
apply.monthly(ac.xts["2015-09/"], mean)

apply.monthly(profit.xts, WinLossRatio)
apply.monthly(profit.xts, maxDrawdown)
apply.monthly(profit.xts, SharpeRatio.annualized)
apply.monthly(profit.xts, Return.cumulative, geometric = F)

#profit.xts.2013 <- profit.xts["2013"]
#chart.CumReturns(profit.xts.2013,geometric = F, ylim = c(-0.2, 0.2), type = "l", 
#                 col = "blue", main = "2013 Cumulative Return Plots of ESN and TF99"
#)
#lines(cumsum(checkData(targetY.xts["2013"], method = "vector")), type = "l", col = "red")
#legend("topright",bty = "n", ncol = 2, cex = 0.5,  legend = c("Standard ESN", "TF99"), col=c("blue","red"), lwd = 2)


#profit.xts.2014 <- profit.xts["2014"]
#chart.CumReturns(profit.xts.2014,geometric = F, ylim = c(-0.2, 0.6), type = "l", 
#                 col = "blue", main = "2014 Cumulative Return Plots of ESN and TF99"
#)
#lines(cumsum(checkData(targetY.xts["2014"], method = "vector")), type = "l", col = "red")
#legend("topright",bty = "n", ncol = 2, cex = 0.5,  legend = c("Standard ESN", "TF99"), col=c("blue","red"), lwd = 2)

profit.xts.2015 <- profit.xts["2015-09/"]
chart.CumReturns(profit.xts.2015,geometric = F, ylim = c(-0.03, 0.03), type = "l", 
                 col = "blue", main = "2015 Cumulative Return Plots of ESN and TF99"
)
lines(cumsum(checkData(targetY.xts["2015-09/"], method = "vector")), type = "l", col = "red")
legend("topright",bty = "n", ncol = 2, cex = 0.5,  legend = c("Standard ESN", "TF99"), col=c("blue","red"), lwd = 2)

