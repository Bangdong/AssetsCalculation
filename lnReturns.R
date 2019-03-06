


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
  


##-----------------  build a function to calculate the return ------------------------##

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






############################### Build a function to calculate the mean and variance for each asset ############################

meanVar <- function(returns){
  
  rows <- dim(returns)[1]
  colums <- dim(returns)[2]
  
  MeanVar.data <- data.frame(Mean = rep(NA, colums),
                             Var = rep(NA, colums)) # create a dataframe to Store n assets' mean and var
  
  rownames(MeanVar.data) <- colnames(returns) # name for each asset returns
  
  for (i in 1 : dim(MeanVar.data)[1]){
    
    MeanVar.data$Mean[i] <- mean(returns[, i])
    MeanVar.data$Var[i] <- sd(returns[, i])
    
  }
  
  MeanVar.data
  
}




##------------------Convert generated data into asset Returns--------------------##

# assetReturns Generator function

simuReturns <- function(copulaSimu, meanVar){
  
  rows <- dim(copulaSimu)[1]
  cols <- dim(copulaSimu)[2]
  
  returnData <- data.frame(matrix( , ncol=cols, nrow=rows))
  
  colnames(returnData) <- colnames(copulaSimu)
  
  
  for (i in 1 : cols){
    
    returnData[,i] <- qnorm(copulaSimu[, i], mean = meanVar[i, 1],
                            sd = meanVar[i, 2], lower.tail = TRUE,
                            log.p = FALSE)
    
  }
  
  
  
  returnData
  
}

