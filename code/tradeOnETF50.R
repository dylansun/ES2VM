rm(list = ls())
source("WinLossRatio.R")
source("ret.R")
require(xts)
require(PerformanceAnalytics)
etf50 <- read.table("../data/510050.txt")
target.etf50 <- etf50$V6[2:length(etf50$V6)] / etf50$V6[1:(length(etf50$V6)-1)] - 1
date.etf50 <- as.Date(as.character(tail(etf50$V1, n = length(target.etf50))), format = "%Y%m%d")
target.etf50.xts <- xts(target.etf50, order.by = date.etf50)

load("../result//slideWinPred_SZ50.RData")
sz50 <- read.table("../data/SH000016.txt")
date.pred <- as.Date(as.character(tail(sz50$V1, n = length(pred))), format = "%Y%m%d")
signal <- unlist(lapply(t(pred), function(x){ifelse(x==0, 0, x/abs(x))}))
pred.xts <- xts(pred, order.by = date.pred)
signal.xts <-  xts(signal, order.by = date.pred)

## Evaluation 
tradetype = "pos"

## ETF 50: obtain profit time series
t.etf50.pred <- as.Date(intersect(time(signal.xts), time(target.etf50.xts)))
profit.etf50 <- rep(0, length(t.etf50.pred))
signal.sub.etf50.xts <- signal.xts[t.etf50.pred]
signal.sub.etf50     <- coredata(signal.xts[t.etf50.pred])
target.sub.etf50.xts <- target.etf50.xts[t.etf50.pred]
target.sub.etf50     <- coredata(target.etf50.xts[t.etf50.pred])

for(i in 1:length(profit.etf50)){
  profit.etf50[i] = ret(signal.sub.etf50[i], target.sub.etf50[i], type = tradetype, tradeloss = 3/10000)
}

profit.etf50.xts <- xts(profit.etf50, order.by = t.etf50.pred) 
backtest.etf50.xts <- xts(profit.etf50, order.by = t.etf50.pred) 
save(backtest.etf50.xts, file = "../result/backtest.etf50.xts.RData")

profit.all.xts <- merge.xts(profit.etf50.xts, target.sub.etf50.xts)
names(profit.all.xts) <- c("ESN", "50 ETF")
## ALL 
round(WinLossRatio(profit.all.xts),4)
print(paste("Max Drawdown = ", round(maxDrawdown(profit.all.xts),4)))
print(paste("Sharpe Ratio = ", round(SharpeRatio.annualized(profit.all.xts), 4)))
print(paste("Final Return = ", round(Return.cumulative(profit.all.xts, geometric = F),4)))

chart.CumReturns(profit.all.xts,geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "axis", main = "Cumulative Return Plots of ESN and 50 ETF")

## By Year
round(apply.yearly(profit.all.xts, WinLossRatio),4)
round(apply.yearly(profit.all.xts, maxDrawdown),4)
round(apply.yearly(profit.all.xts, SharpeRatio.annualized),4)
round(apply.yearly(profit.all.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2013"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2013 Cumulative Return Plots of ESN and 50 ETF")

chart.CumReturns(profit.all.xts["2014"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2014 Cumulative Return Plots of ESN and 50 ETF")

chart.CumReturns(profit.all.xts["2015"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015 Cumulative Return Plots of ESN and 50 ETF")




## By Month
round(apply.monthly(profit.all.xts["2015-11"], WinLossRatio),4)
round(apply.monthly(profit.all.xts["2015-11"], maxDrawdown),4)
round(apply.monthly(profit.all.xts["2015-11"], SharpeRatio.annualized),4)
round(apply.monthly(profit.all.xts["2015-11"], Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2015-11"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015-11 Cumulative Return Plots of ESN and 50 ETF")


