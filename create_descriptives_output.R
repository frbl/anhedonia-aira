create_descriptives_output <- function(files, dirname) {
  dir.create(dirname)
  setwd(dirname)
  for (file in all_loaded_files) {
    temp_name <- strsplit(file$real_file_name, "\\.")[[1]][1]
    sink(paste('summary_', temp_name, '.txt', sep=''))
    a <- summary(file$raw_data)
    print(a)
    sink()
  }
  setwd('../')  
}

