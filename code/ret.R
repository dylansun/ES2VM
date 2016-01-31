ret <- function(signal, rate, type = "all", tradeloss = 0){
  val <- 0
  
  if(signal == 1){
    val <- rate - tradeloss
    if(type == "neg"){
      val <- 0
    }
  }
  if(signal == 0)
  {
    val <- 0
  }
  if(signal == -1){
    val <- -rate/(rate+1) - tradeloss
    if(type == "pos"){
      val <- 0
    }
  }
  return(val)
}

