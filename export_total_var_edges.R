.generate_networks <- function(aira_model) {
  model <- aira_model$var_model
  .set_exo(model)
  aira_output <- AiraOutput$new(aira = aira_model)
  df <- aira_output$.generate_network(autoregressive = TRUE)
  df$name <- aira_model$name

  # Return both the updated model, as well as the result
  list(model = list(aira_model), output = df)
}

.create_pos_neg_group_name <- function(group, positive) {
  negative_name = '(Negative edges)'
  positive_name = '(Positvie edges)'
  name <- paste(group, negative_name, sep=" ")
  if (positive) name <- paste(group, positive_name, sep=" ")
  name
}

.fill_var_matrices <- function(networks_output, groups, options, names, K) {
  result_matrices <- .create_result_matrices(groups, options, names, K)
  noedges <- 0
  count <- 0
  airas_new <- list()

  negative_edges_option <- 'negative_edges'
  positive_edges_option <- 'positive_edges'
  total_sd_option <- 'total-sd'

  for(output in networks_output) {
    count <- count +1
    net <- output$output

    # Use the Aira model, so we can reuse it.
    raw_file_name <- output$model[[1]]$name
    airas_new[[raw_file_name]] <- output$model[[1]]

    # Set the name for the group to put the results in
    group <- groups[1]
    if(net$name %in% anhedonia) group <- groups[2]
    if(net$name %in% no_anhedonia) group <- groups[3]

    if (is.null(net) | is.null(net$links)) {
      noedges <- noedges + 1
      next
    }

    for (i in 1:length(net$links)) {
      # Retrieve the current link
      link <- net$links[i,]

      # In case the link has no source (?) skip
      if (is.na(link$source)) next

      # Get source and tartget name
      source <- net$nodes[net$nodes$index == link$source, 'name']
      target <- net$nodes[net$nodes$index == link$target, 'name']

      # Retrieve the weight of the edge
      weight <- as.numeric(link$weight)

      # Add the current weight to a list, so we can determine the standard deviation next
      cur <- result_matrices[[group]][[total_sd_option]][source, target]
      NonNAindex <- which(is.na(cur[[1]]))
      index <- min(NonNAindex)
      cur[[1]][index] <- weight
      result_matrices[[group]][[total_sd_option]][source, target] <- cur

      cur_total <- result_matrices$total[[total_sd_option]][source, target]
      NonNAindex_total <- which(is.na(cur_total[[1]]))
      index_total <- min(NonNAindex_total)
      cur_total[[1]][index_total] <- weight
      result_matrices$total[[total_sd_option]][source, target] <- cur_total

      row_name <- paste(source,'>', target, sep=" ")
      col_name <- ""

      # Add the rsult to the result matrix (depending on the sign and the group)
      if (sign(as.numeric(weight)) < 0) {
        result_matrices[[group]][[negative_edges_option]][source, target] <- result_matrices[[group]][[negative_edges_option]][source, target] - 1
        result_matrices[['total']][[negative_edges_option]][source, target] <- result_matrices[['total']][[negative_edges_option]][source, target] - 1
        result_matrices[[group]][['total']][source, target] <- result_matrices[[group]][['total']][source, target] + 1
      } else if (sign(as.numeric(weight)) > 0) {
        result_matrices[[group]][[positive_edges_option]][source, target] <- result_matrices[[group]][[positive_edges_option]][source, target] + 1
        result_matrices[['total']][[positive_edges_option]][source, target] <- result_matrices[['total']][[positive_edges_option]][source, target] + 1
        result_matrices[[group]][['total']][source, target] <- result_matrices[[group]][['total']][source, target] + 1
      }

    }
  }
  if(noedges > 0) print(paste(noedges,'networks without edges found (', count, 'total )'))
  list(airas = airas_new, result_matrices = result_matrices)
}

.plot_total_var_networks <- function(result_matrices) {
  plot.new()

  glob_minimum <- 2
  minimum <- glob_minimum
  labels <- dimnames(result_matrices$anhedonia$total)[[2]]

  plottable  <- result_matrices$anhedonia$total
  plot_an_total <- qgraph(plottable, plot= FALSE, minimum = minimum, layout="spring", edge.labels = TRUE, labels = labels)
  plottable  <- result_matrices$no_anhedonia$total
  plot_no_an_total <- qgraph(plottable, plot= FALSE, minimum = minimum, layout="spring", edge.labels = TRUE, labels = labels)

  layout <- averageLayout(plot_an_total, plot_no_an_total)
  qgraph(result_matrices$anhedonia$total,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia Total")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(result_matrices$no_anhedonia$total, vsize=4.5, edge.labels = TRUE, minimum = glob_minimum,  layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia Total")#,nodeNames=bdinms2,legend.cex=0.6)

  qgraph(result_matrices$anhedonia$positive_edges,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia positive edges")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(result_matrices$no_anhedonia$positive_edges, vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia positive edges")#,nodeNames=bdinms2,legend.cex=0.6)

  qgraph(result_matrices$anhedonia$negative_edges,  vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia negative edges")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(result_matrices$no_anhedonia$negative_edges, vsize=4.5, edge.labels = TRUE, minimum = 0, layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia negative edges")#,nodeNames=bdinms2,legend.cex=0.6)

  qgraph(.create_standard_deviation_matrix(result_matrices$anhedonia[['total-sd']], labels),  edge.labels = TRUE, vsize=4.5, minimum = 0, layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia Standard deviation")#,nodeNames=bdinms2,legend.cex=0.6)
  qgraph(.create_standard_deviation_matrix(result_matrices$no_anhedonia[['total-sd']], labels), edge.labels = TRUE,  vsize=4.5, minimum = 0, layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia Standard deviation")#,nodeNames=bdinms2,legend.cex=0.6)
}

.table_total_var_networks <- function(result_matrices, names) {
  # Table to store the results in to export to a csv file
  table_result <- data.frame()

  negative_edges_option <- 'negative_edges'
  positive_edges_option <- 'positive_edges'
  total_sd_option <- 'total-sd'

  anhedonia_column <- 'anhedonia'
  anhedonia_positive_column <- .create_pos_neg_group_name(anhedonia_column, positive = TRUE)
  anhedonia_negative_column <- .create_pos_neg_group_name(anhedonia_column, positive = FALSE)
  anhedonia_count_column <- 'anhedonia_count'
  anhedonia_average_column <- 'anhedonia_average'
  anhedonia_sd_column <- 'anhedonia_sd'
  anhedonia_positive_average_column <- 'anhedonia_positive_average'
  anhedonia_positive_sd_column <- 'anhedonia_positive_sd'
  anhedonia_negative_average_column <- 'anhedonia_negative_average'
  anhedonia_negative_sd_column <- 'anhedonia_negative_sd'

  no_anhedonia_column <- 'no-anhedonia'
  no_anhedonia_positive_column <- .create_pos_neg_group_name(no_anhedonia_column, positive = TRUE)
  no_anhedonia_negative_column <- .create_pos_neg_group_name(no_anhedonia_column, positive = FALSE)
  no_anhedonia_count_column <- 'no-anhedonia_count'
  no_anhedonia_average_column <- 'no-anhedonia_average'
  no_anhedonia_sd_column <- 'no-anhedonia_sd'
  no_anhedonia_positive_average_column <- 'no_anhedonia_positive_average'
  no_anhedonia_positive_sd_column <- 'no_anhedonia_positive_sd'
  no_anhedonia_negative_average_column <- 'no_anhedonia_negative_average'
  no_anhedonia_negative_sd_column <- 'no_anhedonia_negative_sd'

  total_count_column <- 'total_count'
  total_edge_column <- 'total_effect'
  total_edge_average_column <- 'total_edge_average'
  total_edge_sd_column <- 'total_edge_sd'

  table_result['x', anhedonia_column] <- 0
  table_result['x', anhedonia_positive_column] <- 0
  table_result['x', anhedonia_negative_column] <- 0
  table_result['x', anhedonia_count_column] <- 0
  table_result['x', anhedonia_average_column] <- 0
  table_result['x', anhedonia_sd_column] <- 0
  table_result['x', anhedonia_positive_average_column] <- 0
  table_result['x', anhedonia_positive_sd_column] <- 0
  table_result['x', anhedonia_negative_average_column] <- 0
  table_result['x', anhedonia_negative_sd_column] <- 0

  table_result['x', no_anhedonia_column] <- 0
  table_result['x', no_anhedonia_positive_column] <- 0
  table_result['x', no_anhedonia_negative_column] <- 0
  table_result['x', no_anhedonia_count_column] <- 0
  table_result['x', no_anhedonia_average_column] <- 0
  table_result['x', no_anhedonia_sd_column] <- 0
  table_result['x', no_anhedonia_positive_average_column] <- 0
  table_result['x', no_anhedonia_positive_sd_column] <- 0
  table_result['x', no_anhedonia_negative_average_column] <- 0
  table_result['x', no_anhedonia_negative_sd_column] <- 0

  table_result['x', total_count_column] <- 0
  table_result['x', total_edge_column] <- 0
  table_result['x', total_edge_average_column] <- 0
  table_result['x', total_edge_sd_column] <- 0

  for (source in names) {
    for (target in names) {
      row_name <- paste(source, '>', target, sep=" ")

      # Anhedonia
      an_total    <- result_matrices$anhedonia$total[source, target]
      an_positive <- result_matrices$anhedonia[[positive_edges_option]][source, target]
      an_negative <- result_matrices$anhedonia[[negative_edges_option]][source, target]
      an_total_sd <- result_matrices$anhedonia[[total_sd_option]][[source, target]]
      an_total_sd <- an_total_sd[!is.na(an_total_sd) & an_total_sd != 0]

      table_result[row_name, anhedonia_column] <-  an_total
      table_result[row_name, anhedonia_positive_column] <- an_positive
      table_result[row_name, anhedonia_negative_column] <- an_negative
      table_result[row_name, anhedonia_count_column] <- length(an_total_sd)
      table_result[row_name, anhedonia_sd_column] <- sd(an_total_sd)
      table_result[row_name, anhedonia_average_column] <- mean(an_total_sd)
      table_result[row_name, anhedonia_positive_average_column] <- mean(an_total_sd[an_total_sd > 0])
      table_result[row_name, anhedonia_positive_sd_column] <- sd(an_total_sd[an_total_sd > 0])
      table_result[row_name, anhedonia_negative_average_column] <- mean(an_total_sd[an_total_sd < 0])
      table_result[row_name, anhedonia_negative_sd_column] <- sd(an_total_sd[an_total_sd < 0])

      # No-Anhedonia
      no_an_total    <- result_matrices$no_anhedonia$total[source, target]
      no_an_positive <- result_matrices$no_anhedonia[[positive_edges_option]][source, target]
      no_an_negative <- result_matrices$no_anhedonia[[negative_edges_option]][source, target]
      no_an_total_sd <- result_matrices$no_anhedonia[[total_sd_option]][[source, target]]
      no_an_total_sd <- no_an_total_sd[!is.na(no_an_total_sd) & no_an_total_sd != 0]

      table_result[row_name, no_anhedonia_column] <-  no_an_total
      table_result[row_name, no_anhedonia_positive_column] <- no_an_positive
      table_result[row_name, no_anhedonia_negative_column] <- no_an_negative
      table_result[row_name, no_anhedonia_count_column] <- length(no_an_total_sd)
      table_result[row_name, no_anhedonia_sd_column] <- sd(no_an_total_sd)
      table_result[row_name, no_anhedonia_average_column] <- mean(no_an_total_sd)
      table_result[row_name, no_anhedonia_positive_average_column] <- mean(no_an_total_sd[no_an_total_sd > 0])
      table_result[row_name, no_anhedonia_positive_sd_column] <- sd(no_an_total_sd[no_an_total_sd > 0])
      table_result[row_name, no_anhedonia_negative_average_column] <- mean(no_an_total_sd[no_an_total_sd < 0])
      table_result[row_name, no_anhedonia_negative_sd_column] <- sd(no_an_total_sd[no_an_total_sd < 0])

      # Total
      total_sd <- result_matrices$total[[total_sd_option]][[source, target]]
      total_sd <- total_sd[!is.na(total_sd) & total_sd != 0]

      table_result[row_name, total_count_column] <- length(total_sd)
      table_result[row_name, total_edge_sd_column] <- sd(total_sd)
      table_result[row_name, total_edge_average_column] <- mean(total_sd)
    }
  }
  write.csv2(table_result, 'var_edges_overview.csv')
}

export_total_var_edges <- function(airas) {
  networks_output <- mclapply(airas, .generate_networks, mc.cores = detectCores())

  groups <- c('other', 'anhedonia', 'no_anhedonia', 'total')
  options <- c('total','positive_edges', 'negative_edges',
               'total-sd')
  names <- dimnames(airas[[1]]$var_model$y)[[2]]

  #number of variables
  K <- airas[[1]]$var_model$K

  output <- .fill_var_matrices(networks_output, groups, options, names, K)

  .table_total_var_networks(output$result_matrices, names)

  pdf(file="VAR_model_edge_counts(var).pdf")
  .plot_total_var_networks(output$result_matrices)
  dev.off()
  output$airas
}
