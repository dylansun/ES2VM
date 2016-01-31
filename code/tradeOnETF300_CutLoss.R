rm(list = ls())
source("WinLossRatio.R")
source("ret.R")
source("stoploss.R")

## 读入ETF300数据
etf300 <- read.table("../data/510300.txt")

## 差分得到明日涨幅序列
n <- length(etf300$V1)
target.o.etf300 <- etf300$V3[2:n] / etf300$V6[1:(n-1)] - 1
target.h.etf300 <- etf300$V4[2:n] / etf300$V6[1:(n-1)] - 1
target.l.etf300 <- etf300$V5[2:n] / etf300$V6[1:(n-1)] - 1
target.c.etf300 <- etf300$V6[2:n] / etf300$V6[1:(n-1)] - 1
##
## BugFix: 2015-11-2
## label对应的时间应该去掉最后一个,还是应该去掉第一个？
## Example: etf300中含有820个元素, 利用上述差分得到的是前819个元素对应的涨跌幅
## 思考二： 比如p1,p2,p3,..., p820, 得到r1,r2,...,r819, 那么r1表示的是t2时间的涨幅，
## 故时间应该取t2,...,t820
date.etf300 <- as.Date(as.character(tail(etf300$V1, n = length(target.c.etf300))), format = "%Y%m%d")
## date.etf300 <- as.Date(as.character(tail(etf300$V1, n = length(target.etf300))), format = "%Y%m%d")

target.o.etf300.xts <- xts(target.o.etf300, order.by = date.etf300)
target.h.etf300.xts <- xts(target.h.etf300, order.by = date.etf300)
target.l.etf300.xts <- xts(target.l.etf300, order.by = date.etf300)
target.c.etf300.xts <- xts(target.c.etf300, order.by = date.etf300)

load("../result//slideWinPred.RData")
hs300 <- read.table("../data/000300.txt")

##
## BugFix: 2015-11-2
## 预测结果对应的时间是从2013年初到最后
## date.pred <- as.Date(as.character(tail(hs300$V1[], n = length(pred))), format = "%Y%m%d")
date.pred <- as.Date(as.character(tail(hs300$V1[1:(length(hs300$V1))], n = length(pred))), format = "%Y%m%d")

signal <- unlist(lapply(t(pred), function(x){ifelse(x==0, 0, x/abs(x))}))
pred.xts <- xts(pred, order.by = date.pred)
signal.xts <-  xts(signal, order.by = date.pred)

## Evaluation 
tradetype = "all"

## ETF 300: obtain profit time series
t.etf300.pred <- as.Date(intersect(time(signal.xts), time(target.c.etf300.xts)))
profit.etf300 <- rep(0, length(t.etf300.pred))
signal.sub.etf300.xts <- signal.xts[t.etf300.pred]
signal.sub.etf300     <- coredata(signal.xts[t.etf300.pred])

target.sub.o.etf300.xts <- target.o.etf300.xts[t.etf300.pred]
target.sub.o.etf300     <- coredata(target.o.etf300.xts[t.etf300.pred])

target.sub.h.etf300.xts <- target.h.etf300.xts[t.etf300.pred]
target.sub.h.etf300     <- coredata(target.h.etf300.xts[t.etf300.pred])

target.sub.l.etf300.xts <- target.l.etf300.xts[t.etf300.pred]
target.sub.l.etf300     <- coredata(target.l.etf300.xts[t.etf300.pred])

target.sub.c.etf300.xts <- target.c.etf300.xts[t.etf300.pred]
target.sub.c.etf300     <- coredata(target.c.etf300.xts[t.etf300.pred])

for(i in 1:length(profit.etf300)){
  profit.etf300[i] = ret(signal.sub.etf300[i], target.sub.c.etf300[i], type = tradetype, tradeloss = 3/10000)
}

profit.etf300 <- stoploss(profit.etf300,
                     target.sub.o.etf300, 
                     target.sub.h.etf300, 
                     target.sub.l.etf300, 
                     pos.cutloss.threshold = 0.09,
                     neg.cutloss.threshold = 0.09, 
                     tradetype = 'pos')


profit.etf300.xts <- xts(profit.etf300, order.by = t.etf300.pred) 


save(profit.etf300.xts, file = "../result/profit.etf300.cut.Loss.RData")

profit.all.xts <- merge.xts(profit.etf300.xts, target.sub.c.etf300.xts)
names(profit.all.xts) <- c("ESN", "300 ETF")
## ALL 
round(WinLossRatio(profit.all.xts),4)
print(paste("Max Drawdown = ", round(maxDrawdown(profit.all.xts),4)))
print(paste("Sharpe Ratio = ", round(SharpeRatio.annualized(profit.all.xts), 4)))
print(paste("Final Return = ", round(Return.cumulative(profit.all.xts, geometric = F),4)))

chart.CumReturns(profit.all.xts,geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "axis", main = "Cumulative Return Plots of ESN and 300 ETF")

## By Year
round(apply.yearly(profit.all.xts, WinLossRatio),4)
round(apply.yearly(profit.all.xts, maxDrawdown),4)
round(apply.yearly(profit.all.xts, SharpeRatio.annualized),4)
round(apply.yearly(profit.all.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2013"],geometric = F,legend.loc = "topright", wealth.index = T, 
                 colorset =c("blue","red"), main = "2013 Cumulative Return Plots of ESN and 300 ETF")

chart.CumReturns(profit.all.xts["2014"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2014 Cumulative Return Plots of ESN and 300 ETF")

chart.CumReturns(profit.all.xts["2015"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015 Cumulative Return Plots of ESN and 300 ETF")


## By Month
round(apply.monthly(profit.all.xts["2015-10"], WinLossRatio),4)
round(apply.monthly(profit.all.xts["2015-10"], maxDrawdown),4)
round(apply.monthly(profit.all.xts["2015-10"], SharpeRatio.annualized),4)
round(apply.monthly(profit.all.xts["2015-10"], Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2015-10"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015-10 Cumulative Return Plots of ESN and 300 ETF")



