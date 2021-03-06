.generate_total_effect_network <- function(aira_model) {
  # We have to reser the seed because this part is ran in parallel
  set.seed(12345)
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
  name_hedonia_group <- c()

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
    name_hedonia_group <- c(name_hedonia_group, group)

    for (source in rownames(net$network)) {
      for (target in colnames(net$network)) {
        weight <- as.numeric(net$network[source, target])

        # Retrieve the the current standard deviation for the source / target
        # values
        cur <- result_matrices[[group]][['total-sd']][source, target]
        NonNAindex_cur <- which(is.na(cur[[1]]))
        index_cur <- min(NonNAindex_cur)
        cur[[1]][index_cur] <- weight
        result_matrices[[group]][['total-sd']][source, target] <- cur

        # Retrieve the positive and negative values
        if(weight != 0) {
          label  <- ifelse(weight > 0, 'positive', 'negative')
          label_count <- paste(label,'count',sep='_')
          cur_label <- result_matrices[[group]][[label]][source, target]
          result_matrices[[group]][[label]][source, target] <- cur_label + weight
          cur_label_count <- result_matrices[[group]][[label_count]][source, target]
          result_matrices[[group]][[label_count]][source, target] <- cur_label_count + 1
        }

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
  list(airas = airas_new, result_matrices = result_matrices, names=names, name_hedonia_group=name_hedonia_group)
}


.plot_total_effect_networks <- function(result_matrices, groups, participant_names, participant_groups, name_mapping) {
  # Set minimas for the plotting output
  minimum <- 0
  glob_minimum <- 0

  labels <- dimnames(result_matrices$anhedonia$total)[[2]]
  labels <- as.vector(unlist(name_mapping[labels]))
  plot.new()

  #Create the plots for the layout
  plottable  <- result_matrices$anhedonia$total


  lt <- ifelse(plottable > 0, poslinetype, neglinetype)
  plot_an_total <- qgraph(plottable, lty = lt , plot= FALSE, minimum = minimum, groups=groups, layout="circle", edge.labels = TRUE, labels = labels, edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  plottable  <- result_matrices$no_anhedonia$total

  lt <- ifelse(plottable > 0, poslinetype, neglinetype)
  plot_no_an_total <- qgraph(plottable, lty = lt, plot= FALSE, minimum = minimum, groups=groups,layout="circle", edge.labels = TRUE, labels = labels, edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  layout <- 'circle' #averageLayout(plot_an_total, plot_no_an_total)

  # Positief en negatief matrices counts
  maximum = max(result_matrices$anhedonia$positive_count, result_matrices$no_anhedonia$positive_count)
  maximum = max(maximum, result_matrices$anhedonia$negative_count, result_matrices$no_anhedonia$negative_count)

  qgraph(result_matrices$anhedonia$positive_count, edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia positive edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  lt <- ifelse(result_matrices$anhedonia$negative_count * -1 > 0, poslinetype, neglinetype)
  qgraph(result_matrices$anhedonia$negative_count * -1, lty = lt, edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia negative edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  qgraph(result_matrices$no_anhedonia$positive_count,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia positive edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  lt <- ifelse(result_matrices$no_anhedonia$negative_count * -1 > 0, poslinetype, neglinetype)
  qgraph(result_matrices$no_anhedonia$negative_count * -1, lty = lt,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia negative edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)


  # Positief en negatief matrices, the acutal values
  minimum = abs(min(result_matrices$anhedonia$negative, result_matrices$no_anhedonia$negative))
  maximum = max(minimum, result_matrices$anhedonia$positive, result_matrices$no_anhedonia$positive)

  qgraph(result_matrices$anhedonia$positive,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total IRF positive edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  lt <- ifelse(result_matrices$anhedonia$negative > 0, poslinetype, neglinetype)
  qgraph(result_matrices$anhedonia$negative, lty = lt,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total IRF negative edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  qgraph(result_matrices$no_anhedonia$positive,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total IRF positive edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  lt <- ifelse(result_matrices$no_anhedonia$negative > 0, poslinetype, neglinetype)
  qgraph(result_matrices$no_anhedonia$negative, lty = lt,  edge.labels = TRUE, minimum = 0, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total IRF negative edges", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  minimum = abs(min(result_matrices$anhedonia$total, result_matrices$no_anhedonia$total))
  maximum = max(result_matrices$anhedonia$total, result_matrices$no_anhedonia$total, minimum)
  lt <- ifelse(result_matrices$anhedonia$total > 0, poslinetype, neglinetype)
  qgraph(result_matrices$anhedonia$total, lty = lt,  edge.labels = TRUE, minimum = glob_minimum, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  lt <- ifelse(result_matrices$no_anhedonia$total > 0, poslinetype, neglinetype)
  qgraph(result_matrices$no_anhedonia$total, lty = lt, edge.labels = TRUE, minimum = glob_minimum, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  lt <- ifelse(result_matrices$anhedonia$total - result_matrices$no_anhedonia$total > 0, poslinetype, neglinetype)
  qgraph(result_matrices$anhedonia$total - result_matrices$no_anhedonia$total, lty = lt,  edge.labels = TRUE, minimum = minimum, maximum=maximum, groups=groups,layout=layout, posCol="chartreuse3",labels=labels,title="Anhedonia and no anhedonia effects summed, and the difference calculated (an - no an)", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)


  minimum = abs(min(result_matrices$anhedonia$total/length(anhedonia), result_matrices$no_anhedonia$total/length(anhedonia)))
  maximum = max(result_matrices$anhedonia$total/length(anhedonia), result_matrices$no_anhedonia$total/length(anhedonia), minimum)

  lt <- ifelse(result_matrices$anhedonia$total / length(anhedonia) > 0, poslinetype, neglinetype)
  qgraph(result_matrices$anhedonia$total / length(anhedonia), lty = lt,  edge.labels = TRUE, minimum = glob_minimum, maximum = maximum, layout=layout, groups=groups,posCol="chartreuse3",labels=labels,title="Anhedonia total effect summed, and averaged", edge.label.position=glob_label_position, edge.width=glob_edge_width, edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm, vsize=glob_vsize, esize=glob_esize, fade=glob_fade)
  lt <- ifelse(result_matrices$no_anhedonia$total / length(no_anhedonia) > 0, poslinetype, neglinetype)
  qgraph(result_matrices$no_anhedonia$total / length(no_anhedonia), lty = lt, edge.labels = TRUE, minimum = glob_minimum, maximum = maximum,  layout=layout,groups=groups, posCol="chartreuse3",labels=labels,title="No Anhedonia total effect summed, and average", edge.label.position=glob_label_position, edge.width=glob_edge_width, edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm, vsize=glob_vsize, esize=glob_esize, fade=glob_fade)

  plottable_sd_anhedonia <- .create_standard_deviation_matrix(result_matrices$anhedonia[['total-sd']], labels)
  plottable_sd_no_anhedonia <- .create_standard_deviation_matrix(result_matrices$no_anhedonia[['total-sd']], labels)
  minimum = abs(min(plottable_sd_anhedonia, plottable_sd_no_anhedonia))
  maximum = max(plottable_sd_anhedonia, plottable_sd_no_anhedonia, minimum)

  lt <- ifelse(plottable_sd_no_anhedonia > 0, poslinetype, neglinetype)
  qgraph(plottable_sd_no_anhedonia, lty = lt,  edge.labels = TRUE, minimum = glob_minimum, maximum = maximum, layout=layout, groups=groups,posCol="chartreuse3",labels=labels,title="No Anhedonia, Standard deviation of effect", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)
  lt <- ifelse(plottable_sd_anhedonia > 0, poslinetype, neglinetype)
  qgraph(plottable_sd_anhedonia, lty = lt,  edge.labels = TRUE, minimum = glob_minimum, maximum = maximum, layout=layout, groups=groups,posCol="chartreuse3",labels=labels,title="Anhedonia, Standard deviation of effect", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  plottable <- .create_standard_deviation_matrix(result_matrices$total[['total-sd']], labels)
  lt <- ifelse(plottable > 0, poslinetype, neglinetype)
  qgraph(plottable, lty = lt,  edge.labels = TRUE, minimum = glob_minimum, layout=layout,groups=groups, posCol="chartreuse3",labels=labels,title="Total, Standard deviation of effect", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

  # Create a network for each individual participant
  # Create a minimum and maximum for the networks to render (so everything gets scaled well)
  minimum = abs(min(unlist(lapply(result_matrices$total[['total-sd']], min, na.rm=TRUE))))
  maximum = max(unlist(lapply(result_matrices$total[['total-sd']], max, na.rm=TRUE)), minimum)
  in_and_out_strength_matrix <- c()
  anhedonia_index = 0
  no_anhedonia_index = 0


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

    my_minimum = abs(min(plottable))
    my_maximum = abs(max(plottable))
    print(paste("Plotting " , index))
    print(paste('Mymin', my_minimum,'glob',minimum, 'mymax', my_maximum,'glob',maximum))
    current_participant_in_group_index = ifelse(participant_groups[index] == 'anhedonia', anhedonia_index <- anhedonia_index + 1, no_anhedonia_index <- no_anhedonia_index + 1)
    participant_group_nice = participant_groups[index]

    # Remove underscores
    participant_group_nice = gsub("_", " ", participant_group_nice)

    # Capitalize
    participant_group_nice = paste(toupper(substr(participant_group_nice, 1, 1)), substr(participant_group_nice, 2, nchar(participant_group_nice)), sep="")
    title <- paste(participant_group_nice, current_participant_in_group_index)
    lt <- ifelse(plottable > 0, poslinetype, neglinetype)
    qgraph(plottable, lty = lt, edge.labels = TRUE, minimum = glob_minimum, maximum=maximum, layout=layout,groups=groups, posCol="chartreuse3",labels=labels,title=title, edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade)

    # Store the in and out degree in a matrix
    in_and_out_strength_matrix <- .create_in_out_degree_matrix(plottable, participant_names[index], labels,
                                                               current_matrix=in_and_out_strength_matrix)
  }
  print('Exporting in and out strength.')
  write.csv2(data.frame(in_and_out_strength_matrix), file='irf_graphs_in_out_strength.csv')
}

.create_in_out_degree_matrix <- function(adjacency_matrix, participant, col_labels,  current_matrix=c()) {
  instrength <- colSums(abs(adjacency_matrix))
  outstrength <- rowSums(abs(adjacency_matrix))

  current_matrix = rbind(current_matrix, instrength)
  rownames(current_matrix) <- c(head(rownames(current_matrix),-1), paste(participant, 'indegree'))

  current_matrix = rbind(current_matrix, outstrength)
  rownames(current_matrix) <- c(head(rownames(current_matrix),-1), paste(participant, 'outdegree'))

  colnames(current_matrix) <- col_labels
  current_matrix
}

.table_total_effect_networks <- function(result_matrices, names, name_mapping) {
  table_result <- data.frame()
  anhedonia_count_column <- 'anhedonia_count'
  anhedonia_column <- 'anhedonia'
  anhedonia_positive_column <- 'anhedonia_positive'
  anhedonia_positive_count_column <- 'anhedonia_positive_count'
  anhedonia_positive_range_column <- 'anhedonia_positive_range'
  anhedonia_negative_column <- 'anhedonia_negative'
  anhedonia_negative_count_column <- 'anhedonia_negative_count'
  anhedonia_negative_range_column <- 'anhedonia_negative_range'
  anhedonia_average_column <- 'anhedonia_average'
  anhedonia_sd_column <- 'anhedonia_sd'

  no_anhedonia_count_column <- 'no-anhedonia_count'
  no_anhedonia_column <- 'no-anhedonia'
  no_anhedonia_positive_column <- 'no-anhedonia_positive'
  no_anhedonia_positive_count_column <- 'no-anhedonia_positive_count'
  no_anhedonia_positive_range_column <- 'no-anhedonia_positive_range'
  no_anhedonia_negative_column <- 'no-anhedonia_negative'
  no_anhedonia_negative_count_column <- 'no-anhedonia_negative_count'
  no_anhedonia_negative_range_column <- 'no-anhedonia_negative_range'
  no_anhedonia_average_column <- 'no-anhedonia_average'
  no_anhedonia_sd_column <- 'no-anhedonia_sd'

  total_count_column <- 'total_count'
  total_effect_column <- 'total_effect'
  total_effect_average_column <- 'total_effect_average'
  total_effect_sd_column <- 'total_effect_sd'

  table_result['x', anhedonia_column] <- 0
  table_result['x', anhedonia_count_column] <- 0
  table_result['x', anhedonia_positive_column] <- 0
  table_result['x', anhedonia_positive_count_column] <- 0
  table_result['x', anhedonia_positive_range_column] <- 0
  table_result['x', anhedonia_negative_column] <- 0
  table_result['x', anhedonia_negative_count_column] <- 0
  table_result['x', anhedonia_negative_range_column] <- 0
  table_result['x', anhedonia_average_column] <- 0
  table_result['x', anhedonia_sd_column] <- 0

  table_result['x', no_anhedonia_column] <- 0
  table_result['x', no_anhedonia_count_column] <- 0
  table_result['x', no_anhedonia_positive_column] <- 0
  table_result['x', no_anhedonia_positive_count_column] <- 0
  table_result['x', no_anhedonia_positive_range_column] <- 0
  table_result['x', no_anhedonia_negative_column] <- 0
  table_result['x', no_anhedonia_negative_count_column] <- 0
  table_result['x', no_anhedonia_negative_range_column] <- 0
  table_result['x', no_anhedonia_average_column] <- 0
  table_result['x', no_anhedonia_sd_column] <- 0

  table_result['x', total_count_column] <- 0
  table_result['x', total_effect_column] <- 0
  table_result['x', total_effect_average_column] <- 0
  table_result['x', total_effect_sd_column] <- 0

  for (source in names) {
    for (target in names) {
      row_name <- paste(name_mapping[source], '>', name_mapping[target], sep=" ")


      # Anhedonia
      an_total <- result_matrices$anhedonia$total[source, target]
      an_positive <- result_matrices$anhedonia$positive[source, target]
      an_positive_count <- result_matrices$anhedonia$positive_count[source, target]
      an_negative <- result_matrices$anhedonia$negative[source, target]
      an_negative_count <- result_matrices$anhedonia$negative_count[source, target]
      an_total_sd <- result_matrices$anhedonia[['total-sd']][[source, target]]
      an_total_sd <- an_total_sd[!is.na(an_total_sd) & an_total_sd != 0]
      table_result[row_name, anhedonia_column] <-  an_total
      table_result[row_name, anhedonia_count_column] <- length(an_total_sd)
      table_result[row_name, anhedonia_sd_column] <- sd(an_total_sd)
      table_result[row_name, anhedonia_positive_column] <-  an_positive
      table_result[row_name, anhedonia_positive_count_column] <-  an_positive_count
      table_result[row_name, anhedonia_positive_range_column] <-  ifelse(length(an_total_sd[an_total_sd > 0]) > 0, paste(range(an_total_sd[an_total_sd > 0]), collapse=' - '), "0")
      table_result[row_name, anhedonia_negative_column] <-  an_negative
      table_result[row_name, anhedonia_negative_count_column] <-  an_negative_count
      table_result[row_name, anhedonia_negative_range_column] <-  ifelse(length(an_total_sd[an_total_sd < 0]) > 0, paste(range(an_total_sd[an_total_sd < 0]), collapse=' - '), "0")
      table_result[row_name, anhedonia_average_column] <- mean(an_total_sd)

      # No-Anhedonia
      no_an_total <- result_matrices$no_anhedonia$total[source, target]
      no_an_positive <- result_matrices$no_anhedonia$positive[source, target]
      no_an_positive_count <- result_matrices$no_anhedonia$positive_count[source, target]
      no_an_negative <- result_matrices$no_anhedonia$negative[source, target]
      no_an_negative_count <- result_matrices$no_anhedonia$negative_count[source, target]
      no_an_total_sd <- result_matrices$no_anhedonia[['total-sd']][[source, target]]
      no_an_total_sd <- no_an_total_sd[!is.na(no_an_total_sd) & no_an_total_sd != 0]
      table_result[row_name, no_anhedonia_column] <-  no_an_total
      table_result[row_name, no_anhedonia_count_column] <- length(no_an_total_sd)
      table_result[row_name, no_anhedonia_sd_column] <- sd(no_an_total_sd)
      table_result[row_name, no_anhedonia_positive_column] <-  no_an_positive
      table_result[row_name, no_anhedonia_positive_count_column] <-  no_an_positive_count
      table_result[row_name, no_anhedonia_positive_range_column] <-  ifelse(length(no_an_total_sd[no_an_total_sd > 0]) > 0, paste(range(no_an_total_sd[no_an_total_sd > 0]), collapse=' - '), "0")
      table_result[row_name, no_anhedonia_negative_column] <-  no_an_negative
      table_result[row_name, no_anhedonia_negative_count_column] <-  no_an_negative_count
      table_result[row_name, no_anhedonia_negative_range_column] <-  ifelse(length(no_an_total_sd[no_an_total_sd < 0]) > 0, paste(range(no_an_total_sd[no_an_total_sd < 0]), collapse=' - '), "0")
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
  write.csv2(table_result, 'irf_edges_overview.csv')
}

export_total_effect_networks <- function(airas,general_groups, name_mapping) {
  networks_output <- mclapply(airas, .generate_total_effect_network, mc.cores = detectCores())
  groups <- c('other', 'anhedonia', 'no_anhedonia', 'total')
  options <- c('total', 'total-sd', 'positive', 'negative', 'positive_count', 'negative_count')
  names <- dimnames(airas[[1]]$var_model$y)[[2]]

  # Retrieve the number of variables
  K <- airas[[1]]$var_model$K
  output <- .fill_total_effect_matrices(networks_output, groups, options, names, K)

  .table_total_effect_networks(output$result_matrices, names, name_mapping)

  pdf(file="Total_effect_of_one_variable_on_the_other_(irf).pdf")
  .plot_total_effect_networks(output$result_matrices, general_groups, output$names, output$name_hedonia_group, name_mapping)
  dev.off()
  output$airas
}

