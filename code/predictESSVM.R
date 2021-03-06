predictESSVM <- function(model,data,trainLen = 2000,testLen = 1){
  inSize <- model$inSize
  outSize <- model$outSize
  leakingRate <- model$leakingRate
  x <- model$x
  Win <- model$Win
  svm.model <- model$svm.model
  W <- model$W
  data = data[2:length(data)] / data[1:(length(data)-1)] - 1
  data = embed(data, dimension = (inSize+1))
  
  test_X = matrix(data[(trainLen+1) : (trainLen+testLen), 2:(inSize+1)], nrow = testLen, ncol = inSize)
  test_Y = data[(trainLen+1) : (trainLen+testLen), 1]
  
  Y_predict = matrix(0,outSize,testLen)
  
  for (t in 1:testLen){
    u = test_X[t,]
    x = (1-leakingRate)*x + leakingRate*tanh( Win %*% c(1,u) + W %*% x )
    y = predict(svm.model,newdata = t(c(u,x)))
    Y_predict[,t] = y
    # generative mode:
    u = y
  }
  return(Y_predict)
}