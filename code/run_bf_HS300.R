source("bruteforce_exp.R")
data = read.table("../data/SH000300.txt")
alpha = seq(from = 0.1, to = 0.9, by = 0.1)
leak = seq(from = 0.1, to = 0.9, by = 0.1)
ac = matrix(0, nrow = length(alpha), ncol = length(leak))
profit = matrix(0, nrow = length(alpha), ncol = length(leak))
for(i in 1:length(alpha)){
  for(j in 1:length(leak)){
    res = bruteforce_exp(data = data, alpha = alpha[i], leakingRate = leak[j], testidx = 20130000)
    ac[i,j] = res$ac
    profit[i,j] = res$profit
  }
}

save(ac, profit, file = "../result/bf_HS300.RData")

library("R.matlab")
write.csv(ac, file = "../result/bf_HS300_AC.csv")
write.csv(profit, file = "../result/bf_HS300_PROFIT.csv")