library(xts)
library(PerformanceAnalytics)
source("convertToDate.R")
source('getRetXTS.R')
source('WinLossRatio.R')

df <- read.csv("../data/simulation.csv")
time = convertToDate(df$Date)
cumret = as.integer(gsub(df$d_esn_etf300, pattern = ",", replacement = "")) /100000
ret = cumret - c(1,cumret[1:(length(cumret)-1)])
sim_ret.etf300.xts = xts(ret, order.by = time)
save(sim_ret.etf300.xts, file = "../result/sim_ret.etf300.RData")



cumret = as.integer(gsub(df$d_esn_etf500, pattern = ",", replacement = "")) /100000
ret = cumret - c(1,cumret[1:(length(cumret)-1)])
sim_ret.etf500.xts = xts(ret, order.by = time)
save(sim_ret.etf500.xts, file = "../result/sim_ret.etf500.RData")



time = as.Date(as.character(df$Date), format = "%Y%m%d")
cumret = as.integer(gsub(df$d_esn_etf50, pattern = ",", replacement = "")) /100000
ret = cumret - c(1,cumret[1:(length(cumret)-1)])
sim_ret.etf50.xts = xts(ret, order.by = time)
save(sim_ret.etf50.xts, file = "../result/sim_ret.etf50.RData")

## Get the ETF trend
etf300 = read.table("../data/510300.txt")
etf500 = read.table("../data/510500.txt")
etf50 = read.table("../data/510050.txt")

ret.etf300.xts = getRetXTS(etf300) 
ret.etf500.xts = getRetXTS(etf500) 
ret.etf50.xts  = getRetXTS(etf50) 

merge.etf300.xts = merge.xts(sim_ret.etf300.xts, ret.etf300.xts, join = "inner")
merge.etf500.xts = merge.xts(sim_ret.etf500.xts, ret.etf500.xts, join = "inner")
merge.etf50.xts  = merge.xts(sim_ret.etf50.xts, ret.etf50.xts, join = "inner")



load("../result/backtest.etf300.xts.RData")
load("../result/backtest.etf500.xts.RData")
load("../result/backtest.etf50.xts.RData")

merge.etf300.xts = merge.xts(backtest.etf300.xts, merge.etf300.xts, join = "inner")
merge.etf500.xts = merge.xts(backtest.etf500.xts, merge.etf500.xts, join = "inner")
merge.etf50.xts  = merge.xts(backtest.etf50.xts, merge.etf50.xts, join = "inner")



chart.CumReturns(merge.etf300.xts, wealth.index = T, col = c("blue", "red","green"), 
                 legend.loc = "topleft", main = "Cumulative Return Plot on ETF 300")
chart.CumReturns(merge.etf500.xts, wealth.index = T, col = c("blue", "red", "green"),
                 legend.loc = "topleft", main = "Cumulative Return Plot on ETF 500")
chart.CumReturns(merge.etf50.xts , wealth.index = T, col = c("blue", "red", "green"),
                 legend.loc = "topleft", main = "Cumulative Return Plot on ETF 50")



## Statistics 
round(apply.monthly(merge.etf50.xts, WinLossRatio),4)
round(apply.monthly(merge.etf50.xts, maxDrawdown),4)
round(apply.monthly(merge.etf50.xts, SharpeRatio.annualized),4)
round(apply.monthly(merge.etf50.xts, Return.cumulative, geometric = T),4)

## Statistics 
round(apply.monthly(merge.etf300.xts, WinLossRatio),4)
round(apply.monthly(merge.etf300.xts, maxDrawdown),4)
round(apply.monthly(merge.etf300.xts, SharpeRatio.annualized),4)
round(apply.monthly(merge.etf300.xts, Return.cumulative, geometric = T),4)

## Statistics 
round(apply.monthly(merge.etf500.xts, WinLossRatio),4)
round(apply.monthly(merge.etf500.xts, maxDrawdown),4)
round(apply.monthly(merge.etf500.xts, SharpeRatio.annualized),4)
round(apply.monthly(merge.etf500.xts, Return.cumulative, geometric = T),4)


