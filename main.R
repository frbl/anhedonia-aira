# set to directory of script
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

data_file <- "mad_diary_all_update19feb2015_merge_fionneke.csv"

# Use autovar core?
core <- TRUE
run_all <- FALSE
bootstrap_iterations <- 2000
use_100_min_value <- TRUE

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

  removed_columns <- c('not_na_deactivation', 'not_na_activation', 'not_upset', 'na', 'pa',  'time', 'X', 'date')
  autovar_columns <- c( 'pa_activation', 'pa_deactivation', 'na_activation', 'na_deactivation', 'activity', 'upset')


  print(paste('Removing', removed_columns))
  files <- list.files()
  all_loaded_files <<- load_all_files(files, removed_columns = removed_columns)
  setwd("../")


  if(use_100_min_value) {
    #removed_columns <- c('na_deactivation', 'na_activation', 'upset', 'na', 'pa',  'time', 'X', 'date')
    autovar_columns <- c( 'pa_activation', 'pa_deactivation', 'not_na_activation', 'not_na_deactivation', 'activity', 'not_upset')
    for(i in 1:length(all_loaded_files)) {
      data <- all_loaded_files[[i]]$raw_data
      data['not_na_deactivation'] <- 100 - data['na_deactivation']
      data['not_na_activation'] <- 100 - data['na_activation']
      data['not_upset'] <- 100 - data['upset']
      all_loaded_files[[i]]$raw_data <<- data
      print(names(all_loaded_files[[i]]$raw_data))
    }
  }
  print(autovar_columns)

  res <<- list()
  res <<- calculate_all_files(files = all_loaded_files, autovar_columns = autovar_columns)
  good_bad_count(res, anhedonia, no_anhedonia)

  # Remove all invalid models, just to be sure
  res <<- res[paste(good_bad_count(res, anhedonia, no_anhedonia), '.csv',sep="")]
  dirname <- paste('output',format(Sys.time(), "%Y%m%d-%H_%M_%S"), sep='-')
  dir.create(dirname)
  setwd(dirname)

  # Calculate the mean of the variables for each of the groups
  sink('mean_scores.txt')
  calculate_mean_scores(all_loaded_files, anhedonia, no_anhedonia, good_bad_count(res, anhedonia, no_anhedonia))
  sink()

  # Should the var models be exported to a text file?
  export_var_models(res)

  # Should we export plots of the residuals of the var models?
  pdf(file="Standardized residual plots.pdf")
  plot_all(res)
  dev.off()

  # Should we generate the network JSON files of our var models?
  generate_graphs(res)

  # Should we export the 'raw' IRF plots (time - shock plots)
  # generate_irf_graphs(res, bootstrap_iterations)

  # Would we like to run some AIRA methods?
  #if(TRUE || ask_question('AIRA? (N/y): ')) {
  dir.create('aira')
  setwd('aira')

    # Aira models are cached, and also contain a cache themselves. It can take a lot of time to fill those caches, hence, be
    # careful when you regenerate the AIRA 'models'
    #if (TRUE || ask_question('Regenerate AIRA? (N/y): ')) {
  airas <<- initialize_aira(res, bootstrap_iterations)
    #}

  # Remove all invalid models
  airas <<- airas[paste(good_bad_count(res, anhedonia, no_anhedonia),sep='')]

    #if (TRUE || ask_question('Run AIRA? - export var network graph images (N/y): ')) {
  pdf(file="var_models.pdf")
  export_aira_var_network_graphs(airas)
  dev.off()
    #}

    # This is one of the actual AIRA steps. Calculate which of the nodes has the most impact on the other nodes
    #if (TRUE || ask_question('Run AIRA? - determine which of the nodes has the most impact (N/y): ')) {
  airas <<- determine_best_aira_nodes_to_json(airas)
    #}

    # This is one of the actual AIRA steps. Calculate which of the nodes has the most impact on the other nodes
    #if (TRUE || ask_question('Run AIRA? - determine how to reduce stress? (N/y): ')) {
  #airas <<- determine_how_to_reduce_stress(airas)
    #}

    #if (TRUE || ask_question('Aira - Generate overview networks of the number of edges in the VAR models (NO IRF)? (N/y): ')) {

  airas <<- export_total_var_edges(airas)
    #}

    # This is the second of the actual AIRA steps. Generate a network that shows the total effect one of the
    # variables has on the other variables
    #if (TRUE || ask_question('Aira - generate the effect network? (N/y): ')) {
  airas <<- export_total_effect_networks(airas)
    #}
  setwd('../')
  #}
  setwd('../')

  # Stop the clock
  print(proc.time() - start_time)
}

main()
