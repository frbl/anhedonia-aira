.calculate_mean <- function(file) {
  print(paste('Calculating VAR model for:', file$real_file_name))
  autovar_columns <- c(
    # 'pa', 'na',
    'pa_activation', 'pa_deactivation',
    'na_activation', 'na_deactivation',
    'activity', 'stress'
  )
}

add_lists <- function(list1, list2) {
  for (name in names(list2)) {
   list1[[name]] <- list1[[name]] + list2[[name]]
  }
  list1
}


calculate_mean_scores <- function(files, anhedonia, no_anhedonia, valid_models) {
  list_names <- names(all_loaded_files[[1]]$raw_data)
  anhedonia_scores <- list()
  no_anhedonia_scores <- list()
  for (name in list_names) {
   anhedonia_scores[name] <- c()
   no_anhedonia_scores[name] <- c()
  }

  count <- 0
  # Split each of the files in separate dataframes
  for (i in 1:length(files)) {
    file <- files[[i]]
    file_name <- file$real_file_name
    id <- strtoi(strsplit(file_name, "\\.")[[1]][1])
    if (id %in% valid_models) {
      count <- count + 1
      
      # Calculate the mean of a person (all columns)
      if (id %in% anhedonia) {
        anhedonia_scores <- rbind(anhedonia_scores, file$raw_data)
      } else if (id %in% no_anhedonia) {
        no_anhedonia_scores <- rbind(no_anhedonia_scores, file$raw_data)
      }
    }
  }
  
  print('')
  print('Mean and SD Anhedonia')
  print('Mean:')
  print(colMeans(anhedonia_scores))
  print('Standard deviation:')
  print(t(as.matrix(apply(as.matrix(anhedonia_scores), 2, sd))))
  print('')
  print('Mean and SD NO-Anhedonia')
  print('Mean:')
  print(colMeans(no_anhedonia_scores))
  print('Standard deviation:')
  print(t(as.matrix(apply(as.matrix(no_anhedonia_scores), 2, sd))))
  print(paste("Calculated mean over", count, "items"))
}

