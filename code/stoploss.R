## Author: Lili Sun
## last updated: 01-25-2016
## 
##
## =================================
## update: 2016.1.26 
##         add comments 

## profit:   alogrithm profit
## target.o: (open price - pre_close) / pre_close - 1  
## target.h: (high price - pre_close) / pre_close - 1
## target.l: (low  price - pre_close) / pre_close - 1
## signal:   predict signal
## tradetype: 'pos' 
##            'all' 
##            'neg'
## pos.stoploss.threshold:
## neg.stoploss.threshold: 

stoploss <- function(profit,
                     target.o, 
                     target.h, 
                     target.l,
                     signal,
                     pos.stoploss.threshold = 0.09,
                     neg.stoploss.threshold = 0.09, 
                     tradetype = 'pos'){
  
  ## Check input data length
  if(length(target.o)!=length(profit) |
     length(target.h)!=length(profit) |
     length(target.l)!=length(profit)
     )
  {
    cat("Error: Data length Not Match\n")
  }
  
  ## check input threshold
  ## If found negative:  covert to positive
  ##                     show warning message
  if(pos.stoploss.threshold < 0 |
     neg.stoploss.threshold <0 
     )
  {
    cat("Warning: stop loss threshold should be positive.\n");
    cat("Automatically converted to positive");
    pos.stoploss.threshold = abs(pos.stoploss.threshold);
    neg.stoploss.threshold = abs(neg.stoploss.threshold);
  }
  
  
  ## Add stop loss 
  ## if signal =  1, use the low price, and compare to open price 
  ## if signal = -1, use the high price, and compare to open price 
  ## Special case: stop loss at the open position, then use the open price
  for(i in 1:length(profit))
  {
    ## Positive signal, stop loss strategy
    if(signal[i] == 1 & (tradetype == "pos" | tradetype == "all") ){
      
      ## Judge whether stoploss at open price
      ## Note:
      ## if OPEN price change rate is postive, do nothing
      ##                              negtive, check threshold
      ## So, here needs to revert the OPEN price change rate signal
      if(-target.o[i] > pos.stoploss.threshold){
        profit[i] = target.o[i]
      }
      ## If not stop loss at open price
      else{
        ## Judge whether stop loss at low price
        ## Note:
        ## if LOW price change rate is postive, do nothing
        ##                             negtive, check threshold
        ## So, here needs to revert the LOW price change rate signal
        if(-target.l[i] > pos.stoploss.threshold){
          profit[i] = -pos.stoploss.threshold
        }
      }
    }
    
    ## Negtive signal, stop loss strategy
    else if(signal[i] == -1 & (tradetype == "neg" | tradetype == "all")){
      
      ## Judge whether stoploss at open price 
      ## Note:
      ## if open price change rate is negtive, do nothing
      ##                              postive, check threshold
      ## So, here NO needs to revert the open price change rate signal
      if(target.o [i] > neg.stoploss.threshold){
        profit[i] = target.o[i]
      }
      
      else{
        ## Judge whether stop loss at HIGH price 
        ## If not stop loss at open price
        ## Note:
        ## if high price change rate is negtive, do nothing
        ##                              postive, check threshold
        ## So, here NO needs to revert the HIGH price change rate signal
        if(target.h[i] > pos.stoploss.threshold){
          profit[i] = - neg.stoploss.threshold
        }
      }
    }
    ## Zero signal, do nothing
    else{
      ## do nothing
    }
  }
  
  return(profit)
}

