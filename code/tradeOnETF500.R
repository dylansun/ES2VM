source("WinLossRatio.R")
source("ret.R")

etf500 <- read.table("../data/510500.txt")
target.etf500 <- etf500$V6[2:length(etf500$V6)] / etf500$V6[1:(length(etf500$V6)-1)] - 1
date.etf500 <- as.Date(as.character(tail(etf500$V1, n = length(target.etf500))), format = "%Y%m%d")
target.etf500.xts <- xts(target.etf500, order.by = date.etf500)

load("../result//slideWinPred_ZZ500.RData")
ZZ500 <- read.table("../data/SH000905.txt")
date.pred <- as.Date(as.character(tail(ZZ500$V1, n = length(pred))), format = "%Y%m%d")
signal <- unlist(lapply(t(pred), function(x){ifelse(x==0, 0, x/abs(x))}))
pred.xts <- xts(pred, order.by = date.pred)
signal.xts <-  xts(signal, order.by = date.pred)

## Evaluation 
tradetype = "pos"

## ETF 50: obtain profit time series
t.etf500.pred <- as.Date(intersect(time(signal.xts), time(target.etf500.xts)))
profit.etf500 <- rep(0, length(t.etf500.pred))
signal.sub.etf500.xts <- signal.xts[t.etf500.pred]
signal.sub.etf500     <- coredata(signal.xts[t.etf500.pred])
target.sub.etf500.xts <- target.etf500.xts[t.etf500.pred]
target.sub.etf500     <- coredata(target.etf500.xts[t.etf500.pred])

for(i in 1:length(profit.etf500)){
  profit.etf500[i] = ret(signal.sub.etf500[i], target.sub.etf500[i], type = tradetype, tradeloss = 3/10000)
}

profit.etf500.xts <- xts(profit.etf500, order.by = t.etf500.pred) 
backtest.etf500.xts <- xts(profit.etf500, order.by = t.etf500.pred) 
save(backtest.etf500.xts, file = "../result/backtest.etf500.xts.Rdata")

profit.all.xts <- merge.xts(profit.etf500.xts, target.sub.etf500.xts)
names(profit.all.xts) <- c("ESN", "500 ETF")

## ALL 
round(WinLossRatio(profit.all.xts),4)
print(paste("Max Drawdown = ", round(maxDrawdown(profit.all.xts),4)))
print(paste("Sharpe Ratio = ", round(SharpeRatio.annualized(profit.all.xts), 4)))
print(paste("Final Return = ", round(Return.cumulative(profit.all.xts, geometric = F),4)))

chart.CumReturns(profit.all.xts,geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "axis", main = "Cumulative Return Plots of ESN and 500 ETF")

## By Year
round(apply.yearly(profit.all.xts, WinLossRatio),4)
round(apply.yearly(profit.all.xts, maxDrawdown),4)
round(apply.yearly(profit.all.xts, SharpeRatio.annualized),4)
round(apply.yearly(profit.all.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2013"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2013 Cumulative Return Plots of ESN and 500 ETF")

chart.CumReturns(profit.all.xts["2014"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2014 Cumulative Return Plots of ESN and 500 ETF")

chart.CumReturns(profit.all.xts["2015"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015 Cumulative Return Plots of ESN and 500 ETF")


## By Month
round(apply.monthly(profit.all.xts, WinLossRatio),4)
round(apply.monthly(profit.all.xts, maxDrawdown),4)
round(apply.monthly(profit.all.xts, SharpeRatio.annualized),4)
round(apply.monthly(profit.all.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2015-11"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015-11 Cumulative Return Plots of ESN and 500 ETF")



