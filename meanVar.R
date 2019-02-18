

MeanVar <- function(returns){
  
  rows <- dim(returns)[1]
  colums <- dim(returns)[2]
  
  MeanVar.data <- data.frame(Mean = rep(NA, colums),
                             Var = rep(NA, colums)) # create a dataframe to Store n assets' mean and var
  
  
  for (i in 1 : dim(MeanVar.data)[1]){
    
    MeanVar.data$Mean[i] <- mean(returns[, i])
    MeanVar.data$Var[i] <- sd(returns[, i])
    
  }
  
  MeanVar.data
  
}