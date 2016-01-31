## clear old varialbes
rm(list = ls())

source("bruteforce_exp_ESSVM.R")
library("R.matlab")
## Author: Lili Sun
## last updated: 2016-1-27
## Add checkpoints
## Add adaptive result dir
data = read.table("../data/TF99.txt")
alpha = seq(from = 0.1, to = 0.9, by = 0.1)
leak = seq(from = 0.1, to = 0.9, by = 0.1)
ac = matrix(0, nrow = length(alpha), ncol = length(leak))
profit = matrix(0, nrow = length(alpha), ncol = length(leak))
ressize = seq(50, 200, 10 )
testidx = 20150900
testend = 20160100

## Added date: 2016-1-27
## Generate Dir by date
result_dir = paste("../result_TF99_", as.character(Sys.Date()), sep = "");
if(!dir.exists(result_dir)){
  dir.create(result_dir)
}

## write experiments settings to local file,
## named 'exp_settings.txt'
exp_settings = paste(result_dir, "/exp_settings.txt", sep = "")


## Experiment Type
type = c('update','continue')
run_type = type[2];

for(k in 1:16){
  ac_profit_rdata = paste(result_dir,"/bf_",ressize[k],"_ESSVM_TF99.RData", sep = "");
  ac_csv = paste(result_dir,"/bf_",ressize[k],"_ESSVM_TF99_AC.csv", sep = "")
  profit_csv = paste(result_dir,"/bf_",ressize[k] ,"_ESSVM_TF99_PROFIT.csv", sep = "")
  cat('Processing State: ',k,'/', length(ressize),'\n')
  
  ## updated: 2016-1-27
  ## check run type if type == continue
  ## then check whether result file are exists
  ## if all exist, then skip loop
  ## In other cases, re-run the exp
  if(run_type != 'update' && 
     file.exists(ac_profit_rdata) &&
     file.exists(ac_csv) && 
     file.exists(profit_csv)){
    
    ## R uses `next` instead of `continue`
    next;
  }

  ## run single exp
  for(i in 1:length(alpha)){
    for(j in 1:length(leak)){
      res = bruteforce_exp_ESSVM(data = data, alpha = alpha[i], leakingRate = leak[j], 
                           testidx = testidx,testend = testend, resSize = ressize[k])
      ac[i,j] = res$ac
      profit[i,j] = res$profit
    }
  }
  
  ## Save result 
  save(ac, profit, file = ac_profit_rdata)
  write.csv(ac, file = ac_csv)
  write.csv(profit, file = profit_csv)
}

