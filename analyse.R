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
    autovar_data <- autovar_result[[1]]

  } else {
    file <- add_trend(file)
    autovar_data <- var_main(file,
                            vars = autovar_columns,
                            log_level=3,
                            #lag_max=1, # Have a maximum of 1 lag
                            #exogenous_max_iterations=3, # Have a max of 3 exogeneous steps in outlier detection
                            simple_models = TRUE,
                            split_up_outliers = TRUE, # Split up every outlier in its own column
                            include_squared_trend = TRUE,
                            significance = 0.01
                            )

    # Autovar model fitteng could generate a large set of models, cache the models in a caching directory.
    setwd("cache")
    if(length(autovar_data$accepted_models) > 0) {
      print('Exporting model')
      run_export(autovar_data$accepted_models[[1]], file$real_file_name)
    }
    setwd("../")
  }

  # Save the last model found globally.
  last_model <<- autovar_data

  autovar_data$name <- file$real_file_name
  autovar_data
}

calculate_all_files <- function(files, autovar_columns) {
  res <- lapply(files, .run_calculation, autovar_columns = autovar_columns)
  res
}
