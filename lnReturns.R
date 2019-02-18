


##-----------------  a function to calculate the ln based return ------------------------##

returns.calculation <- function(prices){
  
  rows <- dim(prices)[1]
  cols <- dim(prices)[2]
  
  frameReturns <- data.frame(matrix( , ncol=cols, nrow=rows-1))  # Create a dataframe to store calculated returns
  
  colnames(frameReturns) <- colnames(prices) # name the dataframe of returns
  
  for (i in 2 : rows) {
    
    frameReturns[i-1, ] <- log(prices[i, ] / prices[i-1, ], base = exp(1))
    
  }
  
  frameReturns
  
}
  

