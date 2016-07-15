run_export <- function(model, file_name = NULL) {
    model$varest$simple_models <- TRUE
    if(is.null(file_name)) file_name <- model$name 
    
    file_name <- paste(strsplit(file_name, "\\.")[[1]][1], sep=".")
    print(paste("Exporting: ", file_name, sep=""))
    
    # Write output to file
    if(class(model) == 'av_state'){ ## If the output was created from autovar
      dir.create(file_name)
      setwd(file_name)
      dir.create('accepted')
      setwd('accepted')
      i = 0
      print(paste('Accepted models:', length(model$accepted_models)))
      for (acceptedmodel in model$accepted_models) {
        sink(paste(file_name, '_valid_', i,'.txt',sep=''))
        print(var_info(acceptedmodel$varest))  
        sink()
        i<- i + 1
      }
      setwd('../')
      dir.create('rejected')
      setwd('rejected')
      print(paste('Rejected models:', length(model$rejected_models)))
      for (rejected_model in model$rejected_models) {
        sink(paste(file_name, '_invalid_', i,'.txt',sep=''))
        print(var_info(rejected_model$varest))  
        sink()
        i<- i + 1
      }
      setwd('../')
    } else { ## If the output was created from autovar core
      file_name <- paste(file_name, 'txt', sep=".")
      sink(file_name)
      print(paste("Logtransformed: ",model$logtransformed))
      print(paste("Bucket: ", model$bucket))
      print(paste('Lag:', model$varest$p))
      print(var_info(model$varest))  
      sink()
    }
}

run_export_for_more_models <- function(models, provided_file_name = NULL) {
  
  # Write output to file
  i = 1
  for (model in models) {
    if( model == models$name) next
    if(is.null(provided_file_name)) file_name <- models$name
    else file_name <- provided_file_name
    
    file_name <- paste(strsplit(file_name, "\\.")[[1]][1],i, "txt", sep=".")
    print(paste("Exporting: ", file_name, sep=""))
    model$varest$simple_models <- TRUE
    sink(file_name)
    print(paste("Logtransformed: ",model$logtransformed))
    print(paste("Bucket: ", model$bucket))
    print(paste('Lag:', model$varest$p))
    print(var_info(model$varest))
    sink() 
    i <- i + 1
  }
}


export_var_models <- function(var_models) {
  unlink('var_models', recursive = TRUE, force = TRUE)
  dir.create('var_models')
  setwd('var_models')
  #mclapply(var_models, run_export, mc.cores = detectCores())
  for(model in var_models) {
    if(is.vector(model)){
      run_export_for_more_models(model)      
    } else {
      run_export(model) 
    }
  }
  setwd("../")
}
