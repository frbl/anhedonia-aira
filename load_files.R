load_all_files <- function(files, measurements_per_day = 3,
                           removed_columns = c('na', 'pa', 'time', 'X', 'date')){
  file_list <- list()
  i <- 0
  for(file in files){
    print(paste('Loading', file, '(', (i / length(files)) * 100,'%)'))
    file_list[[file]] <- load_file(file,log_level = 3)
    i <- i + 1
  }
  print('Loading 100 %')

  set.seed(12345)
  i<- 0
  for(file_name in files) {
    print(paste('Imputing', file_name, '(', (i / length(files)) * 100,'%)'))
    file <- file_list[[file_name]]

    first_date <- file$data$multiple$date[1]

    # Remove unused columns
    print(paste('Removing', removed_columns))
    file$raw_data <-file$raw_data[,!(names(file$raw_data) %in% removed_columns)]

    file$data[[1]] <- impute_dataframe(file$raw_data, measurements_per_day = measurements_per_day, repetitions = 150)
    file$raw_data <- file$data[[1]]

    file<-set_timestamps(file, date_of_first_measurement=first_date,
                          measurements_per_day = measurements_per_day,
                          log_level=3)

    file_list[[file_name]] <- file

    i <- i + 1
  }
  print('Imputing 100 %')
  file_list
}
