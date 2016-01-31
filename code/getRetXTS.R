require("xts")
source("convertToDate.R")
getRetXTS <- function(data){
  n = length(data$V1)
  ret = data$V6[2:n] / data$V6[1:(n-1)] - 1
  ret.xts = xts(ret, order.by = tail(convertToDate(data$V1), n = n-1))
  return(ret.xts)
}