rm(list = ls())
source("WinLossRatio.R")
source("ret.R")
require(xts)
require(PerformanceAnalytics)

## 读入IF99数据
IF99 <- read.table("../data/IF99.txt")

## 差分得到明日涨幅序列
target.IF99 <- IF99$V6[2:length(IF99$V6)] / IF99$V6[1:(length(IF99$V6)-1)] - 1

##
## BugFix: 2015-11-2
## label对应的时间应该去掉最后一个,还是应该去掉第一个？
## Example: IF99中含有820个元素, 利用上述差分得到的是前819个元素对应的涨跌幅
## 思考二： 比如p1,p2,p3,..., p820, 得到r1,r2,...,r819, 那么r1表示的是t2时间的涨幅，
## 故时间应该取t2,...,t820
date.IF99 <- as.Date(as.character(tail(IF99$V1, n = length(target.IF99))), format = "%Y%m%d")
## date.IF99 <- as.Date(as.character(tail(IF99$V1, n = length(target.IF99))), format = "%Y%m%d")

target.IF99.xts <- xts(target.IF99, order.by = date.IF99)

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

## IF 99: obtain profit time series
t.IF99.pred <- as.Date(intersect(time(signal.xts), time(target.IF99.xts)))
profit.IF99 <- rep(0, length(t.IF99.pred))
signal.sub.IF99.xts <- signal.xts[t.IF99.pred]
signal.sub.IF99     <- coredata(signal.xts[t.IF99.pred])
target.sub.IF99.xts <- target.IF99.xts[t.IF99.pred]
target.sub.IF99     <- coredata(target.IF99.xts[t.IF99.pred])

for(i in 1:length(profit.IF99)){
  profit.IF99[i] = ret(signal.sub.IF99[i], target.sub.IF99[i], type = tradetype, tradeloss = 3/10000)
}

profit.IF99.xts <- xts(profit.IF99, order.by = t.IF99.pred) 
save(profit.IF99.xts, file = "../result/profit.IF99.RData")

profit.all.xts <- merge.xts(profit.IF99.xts, target.sub.IF99.xts)
names(profit.all.xts) <- c("ESN", "IF 99")
## ALL 
round(WinLossRatio(profit.all.xts),4)
print(paste("Max Drawdown = ", round(maxDrawdown(profit.all.xts),4)))
print(paste("Sharpe Ratio = ", round(SharpeRatio.annualized(profit.all.xts), 4)))
print(paste("Final Return = ", round(Return.cumulative(profit.all.xts, geometric = F),4)))

chart.CumReturns(profit.all.xts,geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "axis", main = "Cumulative Return Plots of ESN and IF 99")

## By Year
round(apply.yearly(profit.all.xts, WinLossRatio),4)
round(apply.yearly(profit.all.xts, maxDrawdown),4)
round(apply.yearly(profit.all.xts, SharpeRatio.annualized),4)
round(apply.yearly(profit.all.xts, Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2013"],geometric = F,legend.loc = "topright", wealth.index = T, 
                 colorset =c("blue","red"), main = "2013 Cumulative Return Plots of ESN and IF 99")

chart.CumReturns(profit.all.xts["2014"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"), main = "2014 Cumulative Return Plots of ESN and IF 99")

chart.CumReturns(profit.all.xts["2015"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015 Cumulative Return Plots of ESN and IF 99")


## By Month
round(apply.monthly(profit.all.xts["2015-10"], WinLossRatio),4)
round(apply.monthly(profit.all.xts["2015-10"], maxDrawdown),4)
round(apply.monthly(profit.all.xts["2015-10"], SharpeRatio.annualized),4)
round(apply.monthly(profit.all.xts["2015-10"], Return.cumulative, geometric = T),4)

chart.CumReturns(profit.all.xts["2015-10"],geometric = F,legend.loc = "topleft", wealth.index = T, 
                 colorset =c("blue","red"),begin = "first", main = "2015-10 Cumulative Return Plots of ESN and IF 99")



