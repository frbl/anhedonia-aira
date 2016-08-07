setwd('~/Workspace/frbl/fionneke-aira')
# set to directory of script
#install.packages('Amelia')
#install.packages('devtools')
#install.packages('qgraph')
library('devtools')
#install_github('roqua/autovar')
#install_github('roqua/autovarCore')
#install_github('frbl/airaR', auth_token='ed012aed075dc0dace90b8ad35ecdf7736d29ec9')
unloadNamespace('autovar')
unloadNamespace('aira')

# Load all libraries
library('parallel')
library('autovar',character.only=TRUE)
library('autovarCore')
library('aira')
library('qgraph')

# Source the other R files
source('analyse.R')
source('create_seperate_hgi_csvs.R')
source('export_var_models.R')
source('standardized_residuals.R')
source('plot_with_loop.R')
source('load_files.R')
source('convert_to_graph2.R')
source('run_aira.R')
source('export_total_effect.R')
source('export_total_var_edges.R')
source('generate_irf_plots.R')
source('ask_question_function.R')
source('good_bad_ratio_calculator.R')
source('generate_var_network_images.R')
source('calculate_mean_scores.R')
source('create_descriptives_output.R')

data_file <- "mad_diary_all_update19feb2015_merge_fionneke.csv"

# Use autovar core?
core <- TRUE
run_all <- FALSE
bootstrap_iterations <- 2000
#use_100_min_value <- FALSE
use_positive_model <- TRUE

no_anhedonia <- c(102232,104789,110514,110544,107110,109755,112244,111264,111884,105962,112450,110642,104703,
                  110375,111543,104229,109531,109747,110676,104255,109751,112186,105722,104231,106423)

anhedonia <- c(104517,107596,108172,112028,100849,111939,101912,111745,111779,111464,111958,111473,111566,
               100713,111737,111778,110360,111184,111492,112052,105882,111459,110064,111268,111350)

main <- function() {
  # Time how long it takes
  start_time <- proc.time()

  # Close any sinks if still open (because of a crash or kill in the previous run)
  suppressWarnings(sink())

  # Remove all plots
  if(!is.null(dev.list()) && !is.na(dev.list()['pdf'])) dev.off(dev.list()["pdf"])
  if(!is.null(dev.list()) && !is.na(dev.list()['RStudioGD'])) dev.off(dev.list()["RStudioGD"])

  # Check if we forgot anyone in the list of anhedonia / no anhedonia people
  if(length(c(no_anhedonia, anhedonia)) != 50) {
    print('Lengte van de ids is niet 50')
    warning('Lengte van de ids is niet 50')
  }

  # Split the file into separate CSVs
  create_csvs(data_file=data_file, ids_to_keep = c(no_anhedonia, anhedonia))

  # Generate var models for each individual. These are cached in the global 'res' variable.
  setwd('csv')

  # These columns will be removed before imputation (mainly because of linearity)
  removed_columns <- c('not_na_deactivation', 'not_na_activation', 'not_upset', 'na', 'pa',  'time', 'X', 'date')

  # These columns will be used for the actual var analysis.
  autovar_columns <- c( 'pa_activation', 'pa_deactivation', 'na_activation', 'na_deactivation', 'activity', 'upset')
  groups <- list(c('pa_activation', 'pa_deactivation'), c('na_activation', 'na_deactivation'), c('activity', 'upset'))

  # These are the variables that will be inverted using AIRA
  #negative_variables <- c( 'not_na_activation', 'not_na_deactivation', 'not_upset', 'na_activation', 'na_deactivation', 'upset', 'lnna_activation', 'lnna_deactivation', 'lnupset')
  negative_variables <- c('not_pa_activation', 'not_pa_deactivation', 'not_activity', 'pa_activation', 'pa_deactivation', 'activity', 'lnpa_activation', 'lnpa_deactivation', 'lnactivity')

  #files <- list.files()
  all_loaded_files <<- load_all_files(files, removed_columns = removed_columns)
  setwd("../")


  #type <- ifelse(use_100_min_value, 'positive', 'normal')
  type <- ifelse(use_positive_model, 'positive', 'normal')
  dirname <- paste('output',type,format(Sys.time(), "%Y%m%d-%H_%M_%S"), sep='-')
  dir.create(dirname)
  setwd(dirname)

  #create_descriptives_output(files = all_loaded_files, dirname = 'desciptives_imputed_data')

  # If we want to create columns in which the value is 100 - the original value, we should enable the use_100_min_value option
  # if(use_100_min_value) {
  #   autovar_columns <- c( 'pa_activation', 'pa_deactivation', 'not_na_activation', 'not_na_deactivation', 'activity', 'not_upset')
  #   for(i in 1:length(all_loaded_files)) {
  #     data <- all_loaded_files[[i]]$raw_data
  #
  #     data['not_na_deactivation'] <- 100 - data['na_deactivation']
  #     all_loaded_files[[i]]$data$multiple['not_na_deactivation'] <<- data['not_na_deactivation']
  #
  #     data['not_na_activation'] <- 100 - data['na_activation']
  #     all_loaded_files[[i]]$data$multiple['not_na_activation'] <<- data['not_na_activation']
  #
  #     data['not_upset'] <- 100 - data['upset']
  #     all_loaded_files[[i]]$data$multiple['not_upset'] <<- data['not_upset']
  #
  #     all_loaded_files[[i]]$raw_data <<- data
  #     print(names(all_loaded_files[[i]]$raw_data))
  #   }
  #
  #   create_descriptives_output(files = all_loaded_files, dirname = 'desciptives_positivized_imputed_data')
  # }

  # Change the variable names to NOT_.. if we have a positive model.
  if(use_positive_model) {
    autovar_columns <- c( 'not_pa_activation', 'not_pa_deactivation', 'na_activation', 'na_deactivation', 'not_activity', 'upset')
    groups <- list(c('not_pa_activation', 'not_pa_deactivation'), c('na_activation', 'na_deactivation'), c('not_activity', 'upset'))
    for(i in 1:length(all_loaded_files)) {
      for (positive_name in negative_variables) {
        data <- all_loaded_files[[i]]$raw_data

        data['not_pa_deactivation'] <- data['pa_deactivation']
        all_loaded_files[[i]]$data$multiple['not_pa_deactivation'] <<- data['not_pa_deactivation']

        data['not_pa_activation'] <- data['pa_activation']
        all_loaded_files[[i]]$data$multiple['not_pa_activation'] <<- data['not_pa_activation']

        data['not_activity'] <- data['activity']
        all_loaded_files[[i]]$data$multiple['not_activity'] <<- data['not_activity']

        all_loaded_files[[i]]$raw_data <<- data
        print(names(all_loaded_files[[i]]$raw_data))
      }
    }
  }

  # Res is a list containing the best model generated using autovar core.
  res <<- list()

  # TotalRes is a list containing all models generated using autovarcore (4 models per pariticipant).
  total_res <<- list()
  print('Calculating VAR models for all files')
  total_res <<- calculate_all_files(files = all_loaded_files, autovar_columns = autovar_columns)

  # Select the best model from the 4 models generated by autovarcore
  for (models in total_res) {
    model <- models[[1]]
    model$name <- models$name
    res[[models$name]] <<- model
  }

  # Print the quality of the models
  good_bad_count(res, anhedonia, no_anhedonia)

  # Remove all invalid models
  res <<- res[paste(good_bad_count(res, anhedonia, no_anhedonia), '.csv',sep="")]

  # Calculate the mean of the variables for each of the groups
  sink('mean_scores.txt')
  print('Calculating mean scores for the data')
  calculate_mean_scores(all_loaded_files, anhedonia, no_anhedonia, good_bad_count(res, anhedonia, no_anhedonia))
  sink()

  # Exported the total number of var models to files
  print('Exporting all var models')
  export_var_models(total_res)

  # Create residual plots
  print('Creating residual plots')
  pdf(file="Standardized residual plots.pdf")
  plot_all(res)
  dev.off()

  # Convert the varmodels to positive models
  #print('Converting var models to positive models')
  #print(res[['110544.csv']]$varest$varresult$not_upset)
  #if(use_positive_model) {
    #new_res <- list()
    #for (model in res) {
      #if(is.null(model)) next
      #model$varest <- convert_var_to_positive(model$varest, negative_variables = negative_variables)
      #new_res[[model$name]] <- model
    #}
    #res <<- new_res
  #}
  #print(res[['110544.csv']]$varest$varresult$not_upset)


  # Generate HGI style JSON files of the networks
  print('Exporting json files of valid var models')
  generate_graphs(res)

  dirname <- 'used_csvs'
  dir.create(dirname)
  setwd(dirname)
  print('Exporting the used CSV for the models (with imputation)')
  for (file_data in all_loaded_files) {
    write.table(file_data$raw_data, paste('export', file_data$real_file_name, sep='_'), sep=',')
  }
  setwd('../')

  # Should we export the 'raw' IRF plots (time - shock plots)
  # generate_irf_graphs(res, bootstrap_iterations)

  # Create a directory for the AIRA files (and also some regular var files)
  dir.create('aira')
  setwd('aira')

  # Aira models are cached, and also contain a cache themselves. It can take a lot of time to fill those caches, hence, be
  # careful when you regenerate the AIRA 'models'
  print('Initializing AIRA models')
  airas <<- initialize_aira(res, bootstrap_iterations)

  # Remove all invalid models
  print('Removing invalid models, again')
  airas <<- airas[paste(good_bad_count(res, anhedonia, no_anhedonia),sep='')]

  # Export the VAR models used in a graphical form
  print('Exporting JSONs of the aira models')
  pdf(file="var_models.pdf")
  export_aira_var_network_graphs(airas)
  dev.off()

  # This is one of the actual AIRA steps. Calculate which of the nodes has the most impact on the other nodes
  print('Determining best AIRA nodes for each model')
  airas <<- determine_best_aira_nodes_to_json(airas)

  # This is one of the actual AIRA steps. Calculate which of the nodes has the most impact on the other nodes
  #airas <<- determine_how_to_reduce_stress(airas)

  # Generate overview networks of the number of edges in the VAR models (NO IRF)
  print('Exporting total var edges')
  airas <<- export_total_var_edges(airas)

  # This is the second of the actual AIRA steps. Generate a network that shows the total effect one of the
  # variables has on the other variables
  print('Exporting total effect networks.')
  airas <<- export_total_effect_networks(airas, groups)

  setwd('../')

  setwd('../')

  # Stop the clock
  print(proc.time() - start_time)
}

main()
