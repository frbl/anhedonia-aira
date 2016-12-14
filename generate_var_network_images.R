export_aira_var_network_graphs <- function(airas, name_mapping) {
  plots <- list()
  plot.new()
  glob_minimum <- 0
  i <- 1
  for(aira_model in airas) {
    print(paste('Generating aira ouput network for AIRA model', i , 'of',length(airas)))
    group <- ''
    if(aira_model$name %in% anhedonia) group <- 'Anhedonia'
    if(aira_model$name %in% no_anhedonia) group <- 'No anhedonia'
    aira_output <- AiraOutput$new(aira = aira_model)
    var_network <- aira_output$export_var_network(autoregressive=TRUE)
    nms <- dimnames(var_network)[[2]]
    nms <- as.vector(unlist(name_mapping[nms]))
    plots[[aira_model$name]] <- qgraph(var_network, plot=TRUE, edge.labels = TRUE, layout = "circle", posCol = "chartreuse3", labels = nms, title=paste(group, '(', aira_model$name, ')'), edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade,colfactor=glob_colfactor)

    i<- i + 1
  }
  layout <- averageLayout(plots)
  print('Exporting all plots')
  for(plot in plots) {
    qgraph(plot, glob_vsize, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3", edge.label.position=glob_label_position, edge.width=glob_edge_width,edge.label.cex=glob_edge_label_cex, label.norm=glob_label_norm,vsize=glob_vsize,esize=glob_esize,fade=glob_fade,colfactor=glob_colfactor)
  }
}
