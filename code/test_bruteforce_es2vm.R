source("bruteforce_exp_ESSVM.R")
library("R.matlab")
data = read.table("../data/TF99.txt")
testidx = 20150000
testend = 20160100
resSize = 70
res <- bruteforce_exp_ESSVM(data, initLen = 100,inSize = 5,outSize = 1, resSize = resSize,
                            alpha = 0.6,leakingRate = 0.9,testidx = testidx,testend = testend,
                            tradetype = "all",tradeloss = 0,trainLog = F)
