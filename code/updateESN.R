updateESN <- function(model, method = "Single", newdata ){
  initWMethod <- model$initWMethod 
  trainLen    <- model$trainLen
  initLen     <- model$initLen
  inSize      <- model$inSize
  outSize     <- model$outSize
  resSize     <- model$resSize 
  alpha       <- model$alpha
  leakingRate <- model$leakingRate
  Win         <- model$Win
  W           <- model$W
  x           <- model$x
  Wout        <- model$Wout
  
  
  
  return(model)
}