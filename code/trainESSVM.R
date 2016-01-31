require(quantmod)
require(PerformanceAnalytics)
require(e1071)
source("ESNreservoir.R")
source("ESNreservoirRandom.R")
trainESSVM <- function(data,
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
  Yt = sapply(Yt, function(x){ifelse(x >0, 1, -1)})
  # Generate reservoir  
  set.seed(42)
  
  if(initWMethod == "random"){
    W = ESNreservoirRandom(resSize = resSize,sigma = 0.01, alpha = alpha)
  }
  else{
    W = ESNreservoir(nodesize = resSize, alpha = alpha)  
  }
  
  
  Win = matrix(runif(resSize*(1+inSize),-0.01,0.01),resSize)
  
  #require(R.matlab)
  #con1 = "../result/Win.mat";
  #con2 = "../result/W.mat"
  #writeMat(con1, Win = Win)
  #writeMat(con2, W = W)
  
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
  
  # train a classification model, here use svm 
  X_T = t(X)
  tmp = dim(X_T)
  svm.model <- svm(X_T[,2:tmp[2]], Yt)
  
  # Set the `trainlog` flag to be true for training performance
  # print training error
  # 
  if(trainLog){
    pred = predict(svm.model, newdata = X_T[,2:tmp[2]])
    pred <- sapply(pred, function(x){ifelse(x>0, 1, -1)})
    print(sum(pred == Yt) / length(Yt))
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
  model$svm.model = svm.model
  return(model)
}