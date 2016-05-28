get_standardized_residuals <- function(results) {
  standardized_residuals <- list()
  
  for(i in 1:length(results)) {
    print(i)
    print(results[[i]]$name)
    
    dimension <- dim(resid(results[[i]]$varest))[1]
    residuals <- resid(results[[i]]$varest)[1:dimension,]
    
    standardized_residuals[[results[[i]]$name]] <- scale(residuals, center = FALSE)
    
    # Check whether the standardization was correct
    for(j in 1:dim(residuals)[2]){
      if(!all(residuals[,j] - (standardized_residuals[[results[[i]]$name]][,j] * sd(residuals[,j])) < 0.00000001))
        warning(paste('Something went wrong calculating the standardized residuals', results[[i]]$name))
    }
  }
  
  standardized_residuals
  
}
