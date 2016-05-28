
plot_all <- function(results) {
  
  standardized_residuals <- get_standardized_residuals(results)
  resulting_plots <- c()

  for(i in 1:(length(results))) {
    names <- c()
    
    for(name in dimnames(results[[i]]$varest$y)[[2]] ){
      names <- c(names, name)
      names <- c(names, paste(name, 'squared'))
    }
    
    print(paste(results[[i]]$name, ' (', i, ')', sep=''))
    
    # Don't output the results of the tests
    capture.output({ 
      result <- wntestq(results[[i]]$varest, 0)$passes_test
    })
    
    previous <- ''
    # Loop through all variable names which are not valid (this includes squared ones)
    for(name in names[!result]) {
      validity <- 'Invalid'
      
      # We only use the part without the squared part
      variable_name <<- strsplit(name, " ")[[1]][1]
      # In case the previously analyzed variable is the same as the one with the square, skip it. This only works because these are always consecutive 
      if (previous == variable_name) next
      previous = variable_name
      
      print(paste(variable_name, ', model #', i))
      
      # Get the current residuals and original values, so we can determine the the predicted values
      current_residuals <- resid(results[[i]]$varest)[, variable_name]
      original_values <- results[[i]]$varest$y[, variable_name]
      
      # This calculation of p works for most models, except for the last one, hence the second calculation
      p <- (results[[i]]$varest$p + 1)
      number_of_obs <- length(original_values)
      
      # Remove the first measurements
      original_values <- original_values[p:number_of_obs]
      predicted_values <- original_values + current_residuals
      
      #residuals <- resid(res[[i]]$varest)[, variable_name]
      current_standard_residuals <-standardized_residuals[[results[[i]]$name]][, variable_name]
      
      #print(length(original_values))
      #print(length(predicted_values))
      #print(length(current_residuals))
      #print(length(current_standard_residuals))
      
      data <- data.frame(residuals = current_standard_residuals,
                         original_values = original_values,
                         predicted_values = predicted_values,
                         time = c(p:number_of_obs))
      plot(ggplot(data, aes(x = predicted_values, y= residuals)) +
             scale_x_continuous(name="Predicted values")+
             scale_y_continuous(name="Standardized residuals")+
             ggtitle(paste("Pred vs standardized residuals", variable_name, results[[i]]$name, '(', validity,')'))+
             geom_point())
             #geom_hline())
      
      plot(ggplot(data, aes(x = predicted_values, y = original_values))+
             geom_point()+
             geom_abline()+
             scale_x_continuous(name="Predicted values")+
             scale_y_continuous(name="Actual values")+
             ggtitle(paste("Pred vs actual", variable_name, results[[i]]$name, '(', validity,')')))
      
      # Create QQ plots
      qqnorm(original_values, main = "QQ - Original values")
      qqline(original_values, main = "QQ - Original values")
      
      qqnorm(current_standard_residuals, main = "QQ - Standardized residuals")
      qqline(current_standard_residuals, main = "QQ - Standardized residuals")
    }
  }
}

