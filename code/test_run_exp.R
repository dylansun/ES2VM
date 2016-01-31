source("esn_exp.R")
data <- read.table("../data/SH000905.txt")
alpha = seq(from = 0.1, to = 0.9, by = 0.1)
leakrate = seq(from = 0.1, to = 0.9, by = 0.1)
res_ac = matrix(data = 0, nrow = 9, ncol = 9)
res_ret = matrix(data = 0, nrow = 9, ncol = 9)

for(i in 1:length(alpha)){
  for(j in 1:length(leakrate)){
    res <- esn_exp(data, alpha = alpha[i], leakingRate = leakrate[j])
    res_ac[i,j] = res$ac
    res_ret[i,j] = res$ret
  }
}

save(res_ac, res_ret,file = "../result/ZZTest.RData")
