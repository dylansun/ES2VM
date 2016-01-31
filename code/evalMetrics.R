require("xts")
require("PerformanceAnalytics")
evalMetrics <- function(ret.xts){
  df1 = round(apply.monthly(ret.xts, WinLossRatio),4)
  df2 = round(apply.monthly(ret.xts, maxDrawdown),4)
  df3 = round(apply.monthly(ret.xts, SharpeRatio.annualized),4)
  df4 = round(apply.monthly(ret.xts, Return.cumulative, geometric = T),4)
}