source("bruteforce_exp.R")
library("R.matlab")
data = read.table("../data/TF99.txt")
alpha = seq(from = 0.1, to = 0.9, by = 0.1)
leak = seq(from = 0.1, to = 0.9, by = 0.1)
ac = matrix(0, nrow = length(alpha), ncol = length(leak))
profit = matrix(0, nrow = length(alpha), ncol = length(leak))

ressize = seq(50, 200, 10 )
for(k in 1:16){
  for(i in 1:length(alpha)){
    for(j in 1:length(leak)){
      res = bruteforce_exp(data = data, alpha = alpha[i], leakingRate = leak[j], 
                           testidx = 20150000, resSize = ressize[k])
      ac[i,j] = res$ac
      profit[i,j] = res$profit
    }
  }
  
  save(ac, profit, file = paste("../result_TF99/bf_",ressize[k],"_TF99.RData", sep = ""))
  write.csv(ac, file = paste("../result_TF99/bf_",ressize[k],"_TF99_AC.csv", sep = ""))
  write.csv(profit, file = paste("../result_TF99/bf_",ressize[k] ,"_TF99_PROFIT.csv", sep = ""))
  
}

