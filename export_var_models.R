run_export <- function(model, file_name = NULL) {
    model$varest$simple_models <- TRUE
    if(is.null(file_name)) file_name <- model$name 
    
    file_name <- paste(strsplit(file_name, "\\.")[[1]][1], "txt", sep=".")
    print(paste("Exporting: ", file_name, sep=""))
    
    # Write output to file
    sink(file_name)
    print(paste("Logtransformed: ",model$logtransformed))
    print(paste("Bucket: ", model$bucket))
    print(paste('Lag:', model$varest$p))
    print(var_info(model$varest))
    sink()
}


export_var_models <- function(var_models) {
  unlink('var_models', recursive = TRUE, force = TRUE)
  dir.create('var_models')
  setwd('var_models')
  #mclapply(var_models, run_export, mc.cores = detectCores())
  for( model in var_models) {
    run_export(model)
  }
  setwd("../")
}
