calculate_mean_scores <- function(files, anhedonia, no_anhedonia, valid_models) {
  list_names <- names(all_loaded_files[[1]]$raw_data)
  
  # Create lists to store the data in
  anhedonia_scores <- list()
  anhedonia_means <- list()
  no_anhedonia_scores <- list()
  no_anhedonia_means <- list()
  
  # Initialize the scores with an empty array
  for (name in list_names) {
   anhedonia_scores[name] <- c()
   no_anhedonia_scores[name] <- c()
  }
  
  # Split each of the files in separate dataframes
  for (i in 1:length(files)) {
    file <- files[[i]]
    file_name <- file$real_file_name
    id <- strtoi(strsplit(file_name, "\\.")[[1]][1])
    if (id %in% valid_models) {

      # Create new rows in the empty array (add all data)
      if (id %in% anhedonia) {
        anhedonia_scores <- rbind(anhedonia_scores, file$raw_data)
        anhedonia_means <- rbind(anhedonia_means, unlist(colMeans(file$raw_data)))
      } else if (id %in% no_anhedonia) {
        no_anhedonia_scores <- rbind(no_anhedonia_scores, file$raw_data)
        no_anhedonia_means <- rbind(no_anhedonia_means, unlist(colMeans(file$raw_data)))
      }
    }
  }

  # Calculate all the means of the columns (over all persons for 1 var)
  an_mean <- colMeans(anhedonia_scores)
  an_sd <- as.matrix(apply(as.matrix(anhedonia_scores), 2, sd))
  an_sd_between_persons <- as.matrix(apply(apply(as.matrix(anhedonia_means), 2, unlist),2,sd))

  no_an_mean <- colMeans(no_anhedonia_scores)
  no_an_sd <- as.matrix(apply(as.matrix(no_anhedonia_scores), 2, sd))
  no_an_sd_between_persons <- as.matrix(apply(apply(as.matrix(no_anhedonia_means), 2, unlist),2,sd))
  
  # Create a dataframe with all the data, and return that one
  df <- data.frame('anhedonia_mean'=an_mean, 'anhedonia_sd'=an_sd, 'anhedonia_sd_between_persons'=an_sd_between_persons,
                   'no_anhedonia_mean'=no_an_mean, 'no_anhedonia_sd'=no_an_sd, 'no_anhedonia_sd_between_persons'=no_an_sd_between_persons)
  df
}

