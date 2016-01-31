require(quantmod)
require(PerformanceAnalytics)
source("ESNreservoir.R")
source("ESNreservoirRandom.R")
source("Ret.R")
source("EvalRet.R")
source("ProfitLossRatio.R")
source("AverageProfitLoss.R")
standardESN <- function(data,
                timeStamp,  
                initWMethod = "random",
                trainLen = 2000,
                testLen = 600,
                initLen = 100,
                inSize = 5,
                outSize = 1,
                resSize = 100,
                alpha = 0.8,
                leakingRate = 0.3,
                tradetype = "all"){

  
  data = data[2:length(data)] / data[1:(length(data)-1)] -1
  data = embed(data, dimension = (inSize+1))
  
  train_X = data[1:trainLen, 2:(inSize+1)]
  train_Y = data[1:trainLen, 1]
  Yt = train_Y[(initLen+1):trainLen]
  test_X = data[(trainLen+1) : (trainLen+testLen), 2:(inSize+1)]
  test_Y = data[(trainLen+1) : (trainLen+testLen), 1]
  
  if(testLen == 1){
    test_X = matrix(test_X, nrow = 1, ncol = inSize)
  }
  
  # Generate reservoir

  
  set.seed(42)
  
  if(initWMethod == "random"){
    W = ESNreservoirRandom(resSize = resSize,sigma = 0.01, alpha = alpha)
  }
  else{
    W = ESNreservoir(nodesize = resSize, alpha = alpha)  
  }
  
  
  Win = matrix(runif(resSize*(1+inSize),-0.01,0.01),resSize)
  
  
  
  #leakingRate = 0.3  # leaking rate
  
  # allocated memory for the design (collected states) matrix
  X = matrix(0,1+inSize+resSize,trainLen-initLen)
  # set the corresponding target matrix directly
  #Yt = matrix(data[(initLen+2):(trainLen+1)],1)
  
  # run the reservoir with the data and collect X
  x = rep(0,resSize)
  for (t in 1:trainLen){
    u = train_X[t,]
    x = (1-leakingRate)*x + leakingRate*tanh( Win %*% c(1,u) + W %*% x )
    if (t > initLen)
      X[,t-initLen] = c(1,u,x)
  }
  
  # train the output
  reg = 1e-8  # regularization coefficient
  X_T = t(X)
  Wout = Yt %*% X_T %*% solve( X %*% X_T + reg*diag(1+inSize+resSize) )
  
  # run the trained ESN in a generative mode. no need to initialize here, 
  # because x is initialized with training data and we continue from there.
  Y_predict = matrix(0,outSize,testLen)
  
  for (t in 1:testLen){
    u = test_X[t,]
    x = (1-leakingRate)*x + leakingRate*tanh( Win %*% c(1,u) + W %*% x )
    y = Wout %*% c(1,u,x)
    Y_predict[,t] = y
    # generative mode:
    u = y
    ## this would be a predictive mode:
    #u = data[trainLen+t+1] 
  }
  
  # compute MSE for the first errorLen time steps
  mse = ( sum( (test_Y - Y_predict)^2 )/ testLen )
  print(paste("MSE = ", mse))
  ac <- sum((as.vector(Y_predict)*test_Y) > 0) / testLen
  print(paste("AC = ", ac))
  signal <- unlist(lapply(t(Y_predict), function(x){ifelse(x == 0, 0, x/abs(x))}))
  
  Profit_and_loss_ratio =  Profitlossratio(Y_predict, test_Y, type = tradetype)
  print(paste("Profit Times = ", Profit_and_loss_ratio$profit))
  print(paste("Loss Times = ", Profit_and_loss_ratio$loss))
  print(paste("Profit and Loss Rate = ", Profit_and_loss_ratio$ratio))
  
  profit = rep(0, length(signal))
  for(i in 1:length(signal)){
    profit[i] = ret(signal[i], test_Y[i], type = tradetype, tradeloss = 3/10000)
  }
  
  APL <- AverageProfitLoss(profit)
  print(paste("Average Profit  = ",APL$AverageProfit))
  print(paste("Average Loss    = ",APL$AverageLoss))
  
  ret <- xts(profit, order.by = as.Date(as.character(timeStamp[(trainLen+1):(trainLen+testLen)]), format="%Y%m%d"))
  return(evalRet(profit, ret))

}