export_aira_var_network_graphs <- function(airas, name_mapping) {
  plots <- list()
  plot.new()
  glob_minimum <- 0
  i <- 1
  for(aira_model in airas) {
    print(paste('Generating aira ouput network for AIRA model', i , 'of',length(airas)))
    group <- ''
    if(aira_model$name %in% anhedonia) group <- 'anhedonia'
    if(aira_model$name %in% no_anhedonia) group <- 'no anhedonia'
    aira_output <- AiraOutput$new(aira = aira_model)
    var_network <- aira_output$export_var_network(autoregressive=TRUE)
    nms <- dimnames(var_network)[[2]]
    nms <- as.vector(unlist(name_mapping[nms]))
    plots[[aira_model$name]] <- qgraph(var_network, plot=TRUE, edge.labels = TRUE, vsize = glob_vsize, layout = "circle", posCol = "chartreuse3", labels = nms,
                                       title=paste(group, '(', aira_model$name, ')'))
    i<- i + 1
  }
  layout <- averageLayout(plots)
  print('Exporting all plots')
  for(plot in plots) {
    qgraph(plot, vsize=glob_vsize, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3")#,nodeNames=bdinms2,legend.cex=0.6)
  }
}