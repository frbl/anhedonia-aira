export_aira_var_network_graphs <- function(airas) {
  plots <- list()
  plot.new()
  glob_minimum <- 0
  for(aira_model in airas) {
    group <- ''
    if(aira_model$name %in% anhedonia) group <- 'anhedonia'
    if(aira_model$name %in% no_anhedonia) group <- 'no anhedonia'
    aira_output <- AiraOutput$new(aira = aira_model)
    var_network <- aira_output$export_var_network(autoregressive=TRUE)
    nms <- dimnames(var_network)[[2]]
    plots[[aira_model$name]] <- qgraph(var_network, plot=TRUE, edge.labels = TRUE, vsize = 10, layout = "spring", posCol = "chartreuse3", labels = nms,
                                       title=paste(group, '(', aira_model$name, ')'))
  }
  layout <- averageLayout(plots)
  for(plot in plots) {
    qgraph(plot, vsize=4.5, edge.labels = TRUE, minimum = glob_minimum, layout=layout, posCol="chartreuse3")#,nodeNames=bdinms2,legend.cex=0.6)
  }
}