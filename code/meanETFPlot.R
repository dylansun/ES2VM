rm(list = ls())
require(PerformanceAnalytics)
source("WinLossRatio.R")
load("../result/profit.etf50.RData")
load("../result/profit.etf300.RData")
load("../result/profit.etf500.RData")
all <- merge.xts(profit.etf300.xts, profit.etf50.xts, profit.etf500.xts)
all.m <- checkData(all, method = "matrix")
all.m[is.na(all.m)] <- 0
profit.mean <- (all.m[,1]+all.m[,2]+ all.m[,3])/3
t <- time(all)
k <- cbind(all.m, profit.mean)
k.xts <- xts(k , order.by = t)
chart.CumReturns(k.xts,geometric = F,legend.loc = "topleft", wealth.index = T, 
                 begin = "axis", main = "Cumulative Return Plots of ESN on diff ETF and its mean")
profit.mean.xts <- xts(profit.mean, order.by = t)


## ALL 
round(WinLossRatio(profit.mean.xts),4)
print(paste("Max Drawdown = ", round(maxDrawdown(profit.mean.xts),4)))
print(paste("Sharpe Ratio = ", round(SharpeRatio.annualized(profit.mean.xts), 4)))
print(paste("Final Return = ", round(Return.cumulative(profit.mean.xts, geometric = F),4)))


## By Year
round(apply.yearly(profit.mean.xts, WinLossRatio),4)
round(apply.yearly(profit.mean.xts, maxDrawdown),4)
round(apply.yearly(profit.mean.xts, SharpeRatio.annualized),4)
round(apply.yearly(profit.mean.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(k.xts["2013"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                  main = "2013 Cumulative Return Plots of ESN on diff ETF and its mean")

chart.CumReturns(k.xts["2014"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 main = "2014 Cumulative Return Plots of ESN on diff ETF and its mean")

chart.CumReturns(k.xts["2015"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 main = "2015 Cumulative Return Plots of ESN on diff ETF and its mean")





