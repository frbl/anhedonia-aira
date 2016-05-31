.generate_total_effect_network <- function(aira_model) {
  model <- aira_model$var_model
  .set_exo(model)
  result <- list()
  result[['network']] <- aira_model$determine_effect_network()
  result[['name']] <- aira_model$name

  # Return both the updated model, as well as the result
  list(model = list(aira_model), output = result)
}

.fill_total_effect_matrices <- function(networks_output, groups, options, names, K) {
  result_matrices <- .create_result_matrices(groups, options, names, K)
  noedges <- 0
  count <- 0
  airas_new <- list()
  names <- c()

  # Loop through each of the calculated outputs
  for(output in networks_output) {
    count <- count +1

    net <- output$output

    # Use the Aira model, so we can reuse it.
    raw_file_name <- output$model[[1]]$name
    airas_new[[raw_file_name]] <- output$model[[1]]
    names <- c(names, raw_file_name)

    # Set the name for the group to put the results in
    group <- groups[1]
    if(net$name %in% anhedonia) group <- groups[2]
    if(net$name %in% no_anhedonia) group <- groups[3]

    for (source in rownames(net$network)) {
      for (target in colnames(net$network)) {
        weight<- as.numeric(net$network[source, target])

        # Retrieve the the current standard deviation for the source / target
        # values
        cur <- result_matrices[[group]][['total-sd']][source, target]
        NonNAindex_cur <- which(is.na(cur[[1]]))
        index_cur <- min(NonNAindex_cur)
        cur[[1]][index_cur] <- weight
        result_matrices[[group]][['total-sd']][source, target] <- cur

        cur_total <- result_matrices$total[['total-sd']][source, target]
        NonNAindex_total <- which(is.na(cur_total[[1]]))
        index_total <- min(NonNAindex_total)
        cur_total[[1]][index_total] <- weight
        result_matrices$total[['total-sd']][source, target] <- cur_total
      }
    }

    if (is.null(net$network)) {
      noedges <- noedges + 1
      next
    }

    # Add the complete network to the result matrix
    result_matrices[[group]][['total']] <- result_matrices[[group]][['total']] + net$network
  }
  if(noedges > 0) print(paste(noedges,'networks without edges found'))
  list(airas = airas_new, result_matrices = result_matrices)
}

.plot_total_effect_networks <- function(result_matrices) {
  # Set minimas for the plotting output
  minimum <- 0
  glob_minimum <- 0

  labels <- dimnames(result_matrices$anhedonia$total)[[2]]
  plot.new()

  #Create the plots for the layout
  plottable  <- result_matrices$anhedonia$total
  plot_an_total <- qgraph(plottable, plot= FALSE, minimum = minimum, layout="spring", edge.labels = TRUE, labels = labels)
  plottable  <- result_matrices$no_anhedonia$total
  plot_no_an_total <- qgraph(plottable, plot= FALSE, minimum = minimum, layout="spring", edge.labels = TRUE, labels = labels)
  layout <- averageLayout(plot_an_total, plot_no_an_total)

  qgraph(result_matrices$anhedonia$total,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(result_matrices$no_anhedonia$total, vsize=4.5, edge.labels = TRUE, minimum = glob_minimum,  layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed")#,nodeNames=bdinms2,legend.cex=0.6)

  qgraph(result_matrices$anhedonia$total / length(anhedonia),  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed, and averaged")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(result_matrices$no_anhedonia$total / length(no_anhedonia), vsize=4.5, edge.labels = TRUE, minimum = glob_minimum,  layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed, and average")#,nodeNames=bdinms2,legend.cex=0.6)

  qgraph(result_matrices$anhedonia$total - result_matrices$no_anhedonia$total,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia and no anhedonia effects summed, and the difference calculated (an - no an)")#,nodeNames=bdinms2,legend.cex=0.6)

  plottable <- .create_standard_deviation_matrix(result_matrices$anhedonia[['total-sd']], labels)
  qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  plottable <- .create_standard_deviation_matrix(result_matrices$no_anhedonia[['total-sd']], labels)
  qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  plottable <- .create_standard_deviation_matrix(result_matrices$total[['total-sd']], labels)
  qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Total, Standard deviation of effect")#,nodeNames=bdinms2,legend.cex=0.6)

  # Create a network for each individual participant
  for (index in 1:length(result_matrices$total[['total-sd']][[1]])) {
    K <- dim(result_matrices$total[['total-sd']])[1]
    plottable <- .create_result_matrix(labels, K, 0)
    for(row in 1:K){
      for(col in 1:K){
        current <-result_matrices$total[['total-sd']][[row,col]]
        plottable[row,col] <- current[index]
      }
    }
    if (all(is.na(plottable))) next

    print(paste("Plotting " , index))
    title <- paste("Total effect a variable has on another variable (the significant effects summed)", names[index])
    qgraph(plottable,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title=title)#,nodeNames=bdinms2,legend.cex=0.6)
  }
}

.table_total_effect_networks <- function(result_matrices, names) {
  table_result <- data.frame()
  anhedonia_count_column <- 'anhedonia_count'
  anhedonia_column <- 'anhedonia'
  anhedonia_average_column <- 'anhedonia_average'
  anhedonia_sd_column <- 'anhedonia_sd'

  no_anhedonia_count_column <- 'no-anhedonia_count'
  no_anhedonia_column <- 'no-anhedonia'
  no_anhedonia_average_column <- 'no-anhedonia_average'
  no_anhedonia_sd_column <- 'no-anhedonia_sd'

  total_count_column <- 'total_count'
  total_effect_column <- 'total_effect'
  total_effect_average_column <- 'total_effect_average'
  total_effect_sd_column <- 'total_effect_sd'

  table_result['x', anhedonia_column] <- 0
  table_result['x', anhedonia_count_column] <- 0
  table_result['x', anhedonia_average_column] <- 0
  table_result['x', anhedonia_sd_column] <- 0

  table_result['x', no_anhedonia_column] <- 0
  table_result['x', no_anhedonia_count_column] <- 0
  table_result['x', no_anhedonia_average_column] <- 0
  table_result['x', no_anhedonia_sd_column] <- 0

  table_result['x', total_count_column] <- 0
  table_result['x', total_effect_column] <- 0
  table_result['x', total_effect_average_column] <- 0
  table_result['x', total_effect_sd_column] <- 0

  for (source in names) {
    for (target in names) {
      row_name <- paste(source, '>', target, sep=" ")

      # Anhedonia
      an_total <- result_matrices$anhedonia$total[source, target]
      an_total_sd <- result_matrices$anhedonia[['total-sd']][[source, target]]
      an_total_sd <- an_total_sd[!is.na(an_total_sd) & an_total_sd != 0]
      table_result[row_name, anhedonia_column] <-  an_total
      table_result[row_name, anhedonia_count_column] <- length(an_total_sd)
      table_result[row_name, anhedonia_sd_column] <- sd(an_total_sd)
      table_result[row_name, anhedonia_average_column] <- mean(an_total_sd)

      # No-Anhedonia
      no_an_total <- result_matrices$no_anhedonia$total[source, target]
      no_an_total_sd <- result_matrices$no_anhedonia[['total-sd']][[source, target]]
      no_an_total_sd <- no_an_total_sd[!is.na(no_an_total_sd) & no_an_total_sd != 0]
      table_result[row_name, no_anhedonia_column] <-  no_an_total
      table_result[row_name, no_anhedonia_count_column] <- length(no_an_total_sd)
      table_result[row_name, no_anhedonia_sd_column] <- sd(no_an_total_sd)
      table_result[row_name, no_anhedonia_average_column] <- mean(no_an_total_sd)

      # Total
      total <- result_matrices$total$total[source, target]
      total_sd <- result_matrices$total[['total-sd']][[source, target]]
      total_sd <- total_sd[!is.na(total_sd) & total_sd != 0]
      table_result[row_name, total_effect_column] <-  total
      table_result[row_name, total_count_column] <- length(total_sd)
      table_result[row_name, total_effect_sd_column] <- sd(total_sd)
      table_result[row_name, total_effect_average_column] <- mean(total_sd)
    }
  }
  write.csv(table_result, 'irf_edges_overview.csv')
}

export_total_effect_networks <- function(airas) {
  networks_output <- mclapply(airas, .generate_total_effect_network, mc.cores = detectCores())
  groups <- c('other', 'anhedonia', 'no_anhedonia', 'total')
  options <- c('total', 'total-sd')
  names <- dimnames(airas[[1]]$var_model$y)[[2]]

  # Retrieve the number of variables
  K <- airas[[1]]$var_model$K
  output <- .fill_total_effect_matrices(networks_output, groups, options, names, K)

  .table_total_effect_networks(output$result_matrices, names)

  pdf(file="Total_effect_of_one_variable_on_the_other_(irf).pdf")
  .plot_total_effect_networks(output$result_matrices)
  dev.off()
  output$airas
}
