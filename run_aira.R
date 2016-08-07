library('qgraph')
source('helper_functions.R')

.set_exo <- function(model) {
  # irf(res[[1]]$varest)
  # This is terrible practice. Irf requires the exogen matrix to be available, which is not available anymore.
  # Here we recreate the matrix by padding the datamat with zeros.
  # TODO: Fix this!
  # AIRA also exports this function. Perhaps we could use that one.
  pattern <- paste(dimnames(model$y)[[2]], collapse='|')

  pattern <- paste('const', pattern, sep='|')
  endogen <- grepl(pattern, names(model$datamat), perl=TRUE)
  exo_matrix <<- model$datamat[names(model$datamat)[!endogen]]
  for ( i in 1:model$p) {
    exo_matrix <<- rbind(rep(0, length(!endogen)), exo_matrix)
  }
  assign('lag',model$p, envir = .GlobalEnv )
}


## Initialize the Aira models
.initialize_aira_models <- function(model, iterations){
  filename <- model$name
  filename <- strsplit(filename, "\\.")[[1]][1]
  print(paste("Creating aira placeholder for",filename))
  Aira$new(bootstrap_iterations = iterations, horizon= 10, var_model = model$varest, orthogonalize = FALSE, name=filename)
}

initialize_aira <- function(var_models, iterations) {
  # Using L apply causes 100% cpu for 20 secs +-
  airas_new <- list()
  for(model in var_models){
    airamodel <- .initialize_aira_models(model, iterations)
    airas_new[[airamodel$name]] <- airamodel
  }
  airas_new
}

#############################

## Export all aira networks to JSON
.determine_best_node_to_json <- function(aira_model) {
  #aira_model <- airas[[2]]
  #aira_model <- airas[[1]]
  model <- aira_model$var_model
  .set_exo(model)
  aira_output <- AiraOutput$new(aira = aira_model)
  list(model = list(aira_model), output = aira_output$export_model_to_json())
}

determine_best_aira_nodes_to_json <- function(airas) {
  files <- c()
  print('Determining best aira nodes for all models')
  resulting_json <- mclapply(airas, .determine_best_node_to_json, mc.cores = detectCores())
  print('Done generating, now exporting the jsons')
  airas_new <- list()
  for(i in 1:length(resulting_json)) {
    print(paste('exporting json', i, 'of', length(resulting_json)))
    raw_file_name <-  resulting_json[[i]]$model[[1]]$name
    file_name <- paste(raw_file_name, "json", sep=".")

    airas_new[[raw_file_name]] <- resulting_json[[i]]$model[[1]]

    print(paste('Writing file', file_name))
    json_file <- resulting_json[[i]]$output
    write(json_file, file = file_name)
    files <- c(files, file_name)
  }
  files <- jsonlite::toJSON(files)
  write(files, 'files.json')

  airas_new
}

#.generate_total_effect_network <- function(aira_model) {
  #model <- aira_model$var_model
  #.set_exo(model)
  #result <- list()
  #result[['network']] <- aira_model$determine_effect_network()
  #result[['name']] <- aira_model$name

  ## Return both the updated model, as well as the result
  #list(model = list(aira_model), output = result)
#}

#' This function exports the total effect variables have on each other in a graph form
#' @params airas the airas to use to generate the plots
#' @returns the updates airas model (needed for the caching)
#export_total_effect_networks <- function(airas) {
  #networks_output <- mclapply(airas, .generate_total_effect_network, mc.cores = detectCores())
  #groups <- c('other', 'anhedonia', 'no_anhedonia', 'total')
  #options <- c('total', 'total-sd')
  #names <- dimnames(airas[[1]]$var_model$y)[[2]]

  ## Retrieve the number of variables
  #K <- airas[[1]]$var_model$K
  #result_matrices <- .create_result_matrices(groups, options, names, K)

  #noedges <- 0
  #count <- 0

  #airas_new <- list()
  #names <- c()

  ## Table to store the results in to export to a csv file
  #table_result <- data.frame()
  #networks_output[[1]]$output

  ## Loop through each of the calculated outputs
  #for(output in networks_output) {
    #count <- count +1

    #net <- output$output

    ## Use the Aira model, so we can reuse it.
    #raw_file_name <- output$model[[1]]$name
    #airas_new[[raw_file_name]] <- output$model[[1]]
    #names <- c(names, raw_file_name)
    ## Set the name for the group to put the results in
    #group <- groups[1]
    #if(net$name %in% anhedonia) group <- groups[2]
    #if(net$name %in% no_anhedonia) group <- groups[3]

    #for (source in rownames(net$network)) {
      #for (target in colnames(net$network)) {
        #weight<- as.numeric(net$network[source, target])

        ## Get row and colname to store the current result in
        #row_name <- paste(source, '>', target, sep=" ")
        #col_name <- group

        ## Ensure we have all columnts / rows ready
        #group_effect_column <- paste(col_name,'effect',sep='-')
        #if(!(col_name %in% colnames(table_result))) {
          #table_result['x', 'Total'] <- 0
          #table_result['x', 'Total-effect'] <- 0
          #table_result['x',group_effect_column] <- 0
          #table_result['x',col_name] <- 0
        #}

        #if(!(row_name %in% rownames(table_result))) {
          #table_result[row_name,] <- 0
        #}

        #table_result[is.na(table_result)] <- 0

        ## Retrieve the the current standard deviation for the source / target
        ## values
        #cur <- result_matrices[[group]][['total-sd']][source, target]
        #NonNAindex_cur <- which(is.na(cur[[1]]))
        #index_cur <- min(NonNAindex_cur)
        #cur[[1]][index_cur] <- weight
        #result_matrices[[group]][['total-sd']][source, target] <- cur

        #cur_total <- result_matrices$total[['total-sd']][source, target]
        #NonNAindex_total <- which(is.na(cur_total[[1]]))
        #index_total <- min(NonNAindex_total)
        #cur_total[[1]][index_total] <- weight
        #result_matrices$total[['total-sd']][source, target] <- cur_total

        ## Store the results
        #table_result[row_name, col_name] <- table_result[row_name, col_name] + 1
        #table_result[row_name, group_effect_column] <- table_result[row_name, group_effect_column] + weight
        #table_result[row_name, 'Total'] <- table_result[row_name, 'Total'] + 1
        #table_result[row_name, 'Total-effect'] <- table_result[row_name, 'Total-effect'] + weight
      #}
    #}

    #if (is.null(net$network)) {
      #noedges <- noedges + 1
      #next
    #}

    ## Add the complete network to the result matrix
    #result_matrices[[group]][['total']] <- result_matrices[[group]][['total']] + net$network
  #}
  #if(noedges > 0) print(paste(noedges,'networks without edges found'))

  #write.csv2(table_result, 'irf_edges_overview.csv')

  ## Set minimas for the plotting output
  #minimum <- 0
  #glob_minimum <- 0
  #labels <- dimnames(result_matrices$anhedonia$total)[[2]]
  #plots <- list()
  #plot.new()
  #for(i in 1:length(groups)) {
    #plots[[groups[i]]] <- list()
    #for(j in 1:length(options)) {
      #plottable <- NULL
      #resmat <- result_matrices[[groups[i]]][[options[j]]]
      #if(grepl("^[a-zA-Z_]*-sd",options[j])) {
        #plottable <- .create_standard_deviation_matrix(resmat, labels)
      #} else {
        #plottable <- resmat
      #}
      #plots[[groups[i]]][[options[j]]] <- qgraph(plottable, plot= FALSE, minimum = minimum, layout="spring", edge.labels = TRUE, labels = labels)
    #}
  #}

  #layout <- averageLayout(plots$anhedonia$total, plots$no_anhedonia$total)
  #qgraph(result_matrices$anhedonia$total,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed")#,nodeNames=bdinms2,legend.cex=0.6)
  #qgraph(result_matrices$no_anhedonia$total, vsize=4.5, edge.labels = TRUE, minimum = glob_minimum,  layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed")#,nodeNames=bdinms2,legend.cex=0.6)

  #qgraph(result_matrices$anhedonia$total / length(anhedonia),  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed, and averaged")#,nodeNames=bdinms2,legend.cex=0.6)
  #qgraph(result_matrices$no_anhedonia$total / length(no_anhedonia), vsize=4.5, edge.labels = TRUE, minimum = glob_minimum,  layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed, and average")#,nodeNames=bdinms2,legend.cex=0.6)

  #qgraph(result_matrices$anhedonia$total - result_matrices$no_anhedonia$total,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia and no anhedonia effects summed, and the difference calculated (an - no an)")#,nodeNames=bdinms2,legend.cex=0.6)

  #plottable <- .create_standard_deviation_matrix(result_matrices$anhedonia[['total-sd']], labels)
  #qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  #plottable <- .create_standard_deviation_matrix(result_matrices$no_anhedonia[['total-sd']], labels)
  #qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  #plottable <- .create_standard_deviation_matrix(result_matrices$total[['total-sd']], labels)
  #qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Total, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  ## Create a network for each individual participant
  #for (index in 1:length(result_matrices$total[['total-sd']][[1]])) {
    #K <- dim(result_matrices$total[['total-sd']])[1]
    #plottable <- .create_result_matrix(labels, K, 0)
    #for(row in 1:K){
      #for(col in 1:K){
        #current <-result_matrices$total[['total-sd']][[row,col]]
        #plottable[row,col] <- current[index]
      #}
    #}
    #if (all(is.na(plottable))) next

    #print(paste("Plotting " , index))
    #title <- paste("Total effect a variable has on another variable (the significant effects summed)", names[index])
    #qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title=title)#,nodeNames=bdinms2,legend.cex=0.6)
  #}
  #airas_new
#}

#####################################
#' Function to determine how to reduce stress with 10%.
#' @param airas the list of airas to use
#' @return airas the updated list of airas
.determine_percentage_effect_for_model <- function(aira_model) {
  model <- aira_model$var_model
  .set_exo(model)
  result <- list()
  result[['percentage_effect']] <- aira_model$determine_percentage_effect('stress', -10)
  result[['name']] <- aira_model$name

  # Return both the updated model, as well as the result
  list(model = list(aira_model), output = result)
}
determine_how_to_reduce_stress <- function(airas){
  percentage_output <- mclapply(airas, .determine_percentage_effect_for_model, mc.cores = detectCores())
  result_df <- data.frame()
  airas_new <- list()
  for(output in percentage_output) {
    # Use the Aira model, so we can reuse it.
    raw_file_name <- output$model[[1]]$name
    airas_new[[raw_file_name]] <- output$model[[1]]
    #print(output$output[['percentage_effect']])
    for (col_name in names(output$output$percentage_effect)) {
      if(!(col_name %in% colnames(result_df))) {
        result_df['x',col_name] <- 0
      }

      result_df[raw_file_name, col_name] <-  output$output$percentage_effect[[col_name]]$percentage
    }

  }
  # remove the X column
  result_df <- result_df[-c(1),]
  write.csv2(result_df, 'reducing_stress_percentages.csv')
  airas_new
}

