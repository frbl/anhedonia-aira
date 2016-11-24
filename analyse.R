.run_calculation <- function(file, autovar_columns) {
  print(paste('Calculating VAR model for:', file$real_file_name))

  # Scale the data
  # autovar_data$raw_data <- data.frame(scale(autovar_data$raw_data, scale=FALSE))

  # If use autovar core
  if(core) {
    autovar_result <- autovar(
      file$raw_data,
      selected_column_names = autovar_columns,
      measurements_per_day = 3,
      criterion='BIC',
      test_names = c("portmanteau", "portmanteau_squared","skewness"),
      imputation_iterations = 1
    )
    autovar_data <- autovar_result

  } else {
    file <- add_trend(file)
    
    setwd("log")
    sink(paste('log', file$real_file_name, sep='_'))
    autovar_data <- var_main(file,
                            vars = autovar_columns,
                            log_level=1,
                            criterion='BIC',
                            #lag_max=1, # Have a maximum of 1 lag
                            #exogenous_max_iterations=3, # Have a max of 3 exogeneous steps in outlier detection
                            simple_models = TRUE,
                            split_up_outliers = TRUE, # Split up every outlier in its own column
                            include_squared_trend = TRUE,
                            significance = 0.01
                            )
    sink()
    
    # Autovar model fitteng could generate a large set of models, cache the models in a caching directory.
    setwd("../")
  }

  # Save the last model found globally.
  last_model <<- autovar_data

  # Store the name of the file for later access and return the autovar model.
  autovar_data$name <- file$real_file_name
  autovar_data
}

calculate_all_files <- function(files, autovar_columns) {
  if(!core) {
    dirname <- 'log'
    dir.create(dirname)
  }
  
  res <- lapply(files, .run_calculation, autovar_columns = autovar_columns)
  res
}
