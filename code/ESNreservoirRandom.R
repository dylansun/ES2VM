ESNreservoirRandom <- function(resSize =100,sigma = 0.01, alpha = 0.8){
  W = matrix(runif(resSize*resSize,-0.01,0.01),resSize)
  # Option 1 - direct scaling (quick&dirty, reservoir-specific):
  #W = W * 0.135 
  # Option 2 - normalizing and setting spectral radius (correct, slow):
  #cat('Computing spectral radius...')
  rhoW = abs(eigen(W,only.values=TRUE)$values[1])
  #print('done.')
  W = W * alpha / rhoW
  return(W)
}