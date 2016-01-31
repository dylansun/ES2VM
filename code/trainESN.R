require(quantmod)
require(PerformanceAnalytics)
source("ESNreservoir.R")
source("ESNreservoirRandom.R")
trainESN <- function(data,
                        initWMethod = "random",
                        trainMethod = "lr",
                        trainLen = 2000,
                        initLen = 100,
                        inSize = 5,
                        outSize = 1,
                        resSize = 100,
                        alpha = 0.8,
                        leakingRate = 0.7,
                        trainLog = TRUE
                        ){
  
  
  if(length(data)-1-inSize < trainLen){
    cat("Too small data or too large training length")
    return(-1)
  }
  data = data[2:length(data)] / data[1:(length(data)-1)] -1
  data = embed(data, dimension = (inSize+1))
  
  train_X = data[1:trainLen, 2:(inSize+1)]
  train_Y = data[1:trainLen, 1]
  Yt = train_Y[(initLen+1):trainLen]
  # Generate reservoir  
  set.seed(42)
  
  if(initWMethod == "random"){
    W = ESNreservoirRandom(resSize = resSize,sigma = 0.01, alpha = alpha)
  }
  else{
    W = ESNreservoir(nodesize = resSize, alpha = alpha)  
  }
  
  
  Win = matrix(runif(resSize*(1+inSize),-0.01,0.01),resSize)
  
  require(R.matlab)
  con1 = "../result/Win.mat";
  con2 = "../result/W.mat"
  writeMat(con1, Win = Win)
  writeMat(con2, W = W)
  
  #leakingRate = 0.7 # leaking rate
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
  
  
  
  # Set the `trainlog` flag to be true for training performance
  # print training information 
  # 
  if(trainLog){

    Y_predict = matrix(0,outSize,trainLen)
    
    for (t in 1:trainLen){
      u = train_X[t,]
      x = (1-leakingRate)*x + leakingRate*tanh( Win %*% c(1,u) + W %*% x )
      y = Wout %*% c(1,u,x)
      Y_predict[,t] = y
      # generative mode:
      u = y
      ## this would be a predictive mode:
      #u = data[trainLen+t+1] 
    }
    
    # compute MSE for the first errorLen time steps
    mse = ( sum( (train_Y - Y_predict)^2 )/ trainLen )
    print(paste("Training MSE = ", mse))
    ac <- sum((as.vector(Y_predict)*train_Y) > 0) / trainLen
    print(paste("Training AC = ", ac))
  }
  
  model <- NULL
  model$initWMethod = initWMethod
  model$trainMethod = trainMethod
  model$trainLen = trainLen
  model$initLen = initLen
  model$inSize = inSize
  model$outSize = outSize
  model$resSize = resSize
  model$alpha = alpha
  model$leakingRate = leakingRate
  model$Win = Win
  model$W = W
  model$x = x
  model$Wout = Wout
  return(model)
}