.create_result_matrix = function(names, K, default_value = 0) {
  result_matrix <- matrix(rep(default_value,K*K), nrow=K, ncol=K)
  colnames(result_matrix) <- names
  rownames(result_matrix) <- names
  result_matrix
}

.create_result_matrices = function(groups, options, names, K) {
  result_matrices <- list()
  for(i in 1:length(groups)) {
    result_matrices[[groups[i]]] <- list()
    for(j in 1:length(options)) {
      default_value <- ifelse(grepl("^[a-zA-Z_]*-sd",options[j]), list(rep(NA,50)), 0)
      result_matrices[[groups[i]]][[options[j]]] <- .create_result_matrix(names, K, default_value=default_value)
    }
  }
  result_matrices
}

std_dev_func <- function(x) sd(x, na.rm=TRUE)

.create_standard_deviation_matrix <- function(resmat, labels) {
  # Standard deviation function to use
  
  plottable <- .create_result_matrix(labels, dim(resmat)[2], default_value=0)
  for(r in 1:dim(plottable)[1])
    for(c in 1:dim(plottable)[2]){
      plottable[r,c] <- std_dev_func(resmat[r,c][[1]])
    }
  
  plottable[is.na(plottable)] <- 0
  plottable
}

.create_result_table <- function(groups) {
  table_result <- data.frame()
  
  # Create an x row with all columns. The x will be removed later on.
  table_result['x', 'Total'] <- 0
  table_result['x', 'Total-average'] <- 0
  table_result['x', 'Total-sd'] <- list(rep(NA,50))
  
  for(group in groups) {
    if(group == 'other') next    
    col_name <- paste(group, '(Negative edges)', sep=" ")
    avgcolname <- paste(col_name,'average',sep='-')
    sdcolname <- paste(col_name,'sd',sep='-')
    table_result['x', col_name] <- 0
    table_result['x', avgcolname] <- 0
    table_result['x', sdcolname] <- list(rep(NA,50))
    
    col_name <- paste(group, '(Positive edges)', sep=" ")
    avgcolname <- paste(col_name,'average',sep='-')
    sdcolname <- paste(col_name,'sd',sep='-')
    table_result['x', col_name] <- 0
    table_result['x', avgcolname] <- 0
    table_result['x', sdcolname] <- list(rep(NA,50))
  }
  table_result
}