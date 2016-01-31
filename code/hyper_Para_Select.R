category = "TF99"
method = "bf"
ressize = seq(50,200,10)
# avg_ac = rep(0, length(ressize))
# avg_profit = rep(0, length(ressize))
# for(i in 1: length(ressize)){
#   basepath = "../result_TF99/";
#   rdata = paste(basepath,method, "_", ressize[i],"_",category, ".RData", sep = "")
#   load(rdata)
#   avg_ac[i] = mean(ac)
#   avg_profit[i] = mean(profit)
# }

max_res = 0
max_index = c(0,0)

max_valid_profit = 0
for(i in 1: length(ressize)){
  basepath = "../result_TF99/";
  rdata = paste(basepath,method, "_", ressize[i],"_",category, ".RData", sep = "")
  load(rdata)
  temp_max = max(profit)
  if(temp_max > max_valid_profit){
    max_res = ressize[i]
    max_index = which(profit == temp_max, arr.ind = TRUE)
    max_valid_profit = temp_max
  }
}