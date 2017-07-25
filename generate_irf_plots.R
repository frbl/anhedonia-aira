.generate_single_image <- function(model, iterations, orthogonal){
  # Resetting the seed, as this is run multicore and is non deterministic because of the bootstrap
  set.seed(12345)
  print('Generating new model')
  .set_exo(model$varest)
  bootstrap = FALSE
  if(iterations > 0) {
    bootstrap = TRUE
  }
  vars::irf(model$varest, n.ahead = 10, ortho = orthogonal, boot = bootstrap, runs = iterations)
}

generate_irf_graphs <- function(var_models, iterations) {
  plots <<- mclapply(var_models, .generate_single_image, mc.cores = detectCores(), iterations, FALSE)
  plots_ortho <<- mclapply(var_models, .generate_single_image, mc.cores = detectCores(), iterations, TRUE)
  for (i in 1:length(plots)) {
    irf_data = plots[[i]]
    irf_data_ortho = plots_ortho[[i]]
    title=names(plots[i])
    devAskNewPage(FALSE)
    options(device.ask.default = FALSE)
    grDevices::devAskNewPage(ask=FALSE)
    pdf(file=paste(title,".pdf", sep=''))
    plot(irf_data)
    plot(irf_data_ortho)
    dev.off()
  }
}

