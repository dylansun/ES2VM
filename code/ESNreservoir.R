ESNreservoir <- function(nodesize = 100, alpha = 0.8){
  topright = -alpha^nodesize
  #      0 0 0 ... 0 -a^N
  #      1 0 0 ... 0  0
  # w =  0 1 0 ... 0  0
  #      0 0 1 ... 0  0
  #      0 0 0 ... 0  0
  #      0 0 0 ... 1  0
  W = diag(nodesize-1)
  W = cbind(W, rep(0, nodesize -1))
  W = rbind(c(rep(0,nodesize -1 ), topright), W)
  return(W)
}