.convert_to_graph2 <- function(av_state, net_cfg, forced_variable = NULL) {
  rnames <- NULL
  nodedata <- NULL
  linkdata <- NULL
  nodecount <- 0
  nodedegree <- list()
  res <- av_state$accepted_models[[1]]$varest$varresult
  var_names <- names(res)
  i <- 0
  for (varname in var_names) {
    nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                          name=format_property_name(unprefix_ln(varname),net_cfg),
                                          type=format_property_type(unprefix_ln(varname),net_cfg),
                                          stringsAsFactors=FALSE))
    rnames <- c(rnames,varname)
    nodecount <- nodecount+1
  }
  nodedatac <- nodedata
  nodecountc <- nodecount
  rnamesc <- rnames
  # create a data.frame of significant connections, order it by coefficient strength
  linkstr <- NULL
  # dynamic eqs
  for (equation in res) {
    i <- i+1
    eqsum <- summary(equation)
    eqname <- var_names[i]
    for (fromnodename in var_names) {
      if (!(paste(fromnodename, ".l1",sep = "") %in% rownames(coef(eqsum)))) next
      if (fromnodename == eqname) next
      p_val <- eqsum$coefficients[paste(fromnodename,'.l1',sep=""),4]
      if (p_val > 0.05) next
      nodedegree[[fromnodename]] <- ifelse(is.null(nodedegree[[fromnodename]]),0,nodedegree[[fromnodename]]) + 1
      nodedegree[[eqname]] <- ifelse(is.null(nodedegree[[eqname]]),0,nodedegree[[eqname]]) + 1
      coef <- eqsum$coefficients[paste(fromnodename,'.l1',sep=""),1]
      linkstr <- rbind(linkstr,data.frame(source=unprefix_ln(fromnodename),
                                          target=unprefix_ln(eqname),
                                          coef=abs(coef),
                                          sign=sign(coef),
                                          stringsAsFactors=FALSE))
      for (varname in c(eqname,fromnodename)) {
        if (varname %in% rnames) next
        nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                              name=format_property_name(unprefix_ln(varname),net_cfg),
                                              type=format_property_type(unprefix_ln(varname),net_cfg),
                                              stringsAsFactors=FALSE))
        rnames <- c(rnames,varname)
        nodecount <- nodecount+1
      }
      tonode<- which(eqname == rnames) -1
      fromnode <- which(fromnodename == rnames) -1
      linkdata <- rbind(linkdata,data.frame(source=fromnode,
                                            target=tonode,
                                            coef=toString(coef),
                                            stringsAsFactors=FALSE))
    }
  }
  if (!is.null(linkstr))
    linkstr <- linkstr[with(linkstr,order(linkstr$coef,decreasing=TRUE)),]
  # get a list of nodes with maximum node degree
  maxdeg <- 0
  for (degree in nodedegree)
    maxdeg <- max(maxdeg,degree)
  maxdeg_nodes <- NULL
  for (varname in names(nodedegree))
    if (nodedegree[[varname]] == maxdeg)
      maxdeg_nodes <- c(maxdeg_nodes,unprefix_ln(varname))
  # contemp eqs
  linkdatac <- NULL
  signmat <- significance_matrix(summary(av_state$accepted_models[[1]]$varest))
  n <- length(var_names)
  for (i in 1:(n-1))
    for (j in (i+1):n) {
      if (signmat[j*2,i] > correlation_significance() || signmat[j*2-1,i] == 0) next
      for (varname in c(var_names[[i]],var_names[[j]])) {
        if (varname %in% rnamesc) next
        nodedatac <- rbind(nodedatac,data.frame(index=nodecountc,
                                                name=format_property_name(unprefix_ln(varname),net_cfg),
                                                type=format_property_type(unprefix_ln(varname),net_cfg),
                                                stringsAsFactors=FALSE))
        rnamesc <- c(rnamesc,varname)
        nodecountc <- nodecountc+1
      }
      tonode   <- which(var_names[[i]] == rnamesc) -1
      fromnode <- which(var_names[[j]] == rnamesc) -1
      linkdatac <- rbind(linkdatac,data.frame(source=fromnode,
                                              target=tonode,
                                              coef=toString(signmat[j*2-1,i]),
                                              stringsAsFactors=FALSE))
    }
  # generate textual dynamic graph summary (array of arrays)
  graphsum <- NULL
  # first connection is the strongest one of the maxdeg_nodes
  nr_links <- 0
  if (!is.null(linkstr))
    nr_links <- dim(linkstr)[1]
  usedlinks <- list()
  usedlinks[1:nr_links] <- 0
  seen_positive_target <- FALSE
  for (i in 1:nr_links) {
    if (i > nr_links) break
    if (linkstr[i,]$source %in% maxdeg_nodes || linkstr[i,]$target %in% maxdeg_nodes) {
      graphsum <- rbind(graphsum,data.frame(source=linkstr[i,]$source,
                                            target=linkstr[i,]$target,
                                            sign=linkstr[i,]$sign,
                                            stringsAsFactors=FALSE))
      usedlinks[[i]] <- 1
      if (is_positive_property(linkstr[i,]$target,net_cfg))
        seen_positive_target <- TRUE
      break
    }
  }
  # second connection is the strongest one that hasn't been used yet
  for (i in 1:nr_links) {
    if (i > nr_links) break
    if (usedlinks[[i]] == 1) next
    graphsum <- rbind(graphsum,data.frame(source=linkstr[i,]$source,
                                          target=linkstr[i,]$target,
                                          sign=linkstr[i,]$sign,
                                          stringsAsFactors=FALSE))
    usedlinks[[i]] <- 1
    if (is_positive_property(linkstr[i,]$target,net_cfg))
      seen_positive_target <- TRUE
    break
  }

  # two scenarios for the third connection:
  if (is.null(forced_variable) || is.null(net_cfg$incident_to_best_of)) {
    # if all used so far had a negative target, this target has to be positive.
    for (i in 1:nr_links) {
      if (i > nr_links) break
      if (usedlinks[[i]] == 1) next
      if (!seen_positive_target && !is_positive_property(linkstr[i,]$target,net_cfg)) next
      graphsum <- rbind(graphsum,data.frame(source=linkstr[i,]$source,
                                            target=linkstr[i,]$target,
                                            sign=linkstr[i,]$sign,
                                            stringsAsFactors=FALSE))
      usedlinks[[i]] <- 1
      break
    }
  } else {
    # find the strongest unused link that starts at one of the variables in net_cfg$incident_to_best_of
    # and ends at the forced_variable variable
    for (i in 1:nr_links) {
      if (i > nr_links) break
      if (usedlinks[[i]] == 1) next
      if (linkstr[i,]$target != forced_variable || !(linkstr[i,]$source %in% net_cfg$incident_to_best_of)) next
      graphsum <- rbind(graphsum,data.frame(source=linkstr[i,]$source,
                                            target=linkstr[i,]$target,
                                            sign=linkstr[i,]$sign,
                                            stringsAsFactors=FALSE))
      usedlinks[[i]] <- 1
      break
    }
  }
  result <- paste('[',
                  toString(toJSON(list(
                                       links = linkdata,nodes = nodedata
                                       ))),     # dynamic
                  ',',
                  toString(toJSON(list(
                                       links = linkdatac,nodes = nodedatac
                                       ))),   # contemporaneous
                  ',',
                  toString(toJSON(graphsum)), sep = "")
  if (!is.null(net_cfg$include_model) && net_cfg$include_model) {
    exogen_mat <- exogen_matrix(av_state$accepted_models[[1]]$varest)
    result <- paste(result, ',',
                    toString(toJSON( digits = 20, x= list(
                                                          endogen = list(
                                                                         header = colnames(av_state$accepted_models[[1]]$varest$y),
                                                                         body = as.matrix(av_state$accepted_models[[1]]$varest$y)
                                                                         ),
                                                          exogen = list(
                                                                        header = colnames(exogen_mat),
                                                                        body = as.matrix(exogen_mat)
                                                                        ),
                                                          coefs = list(
                                                                       header = colnames(coef(av_state$accepted_models[[1]]$varest)[[1]]),
                                                                       body = lapply(coef(av_state$accepted_models[[1]]$varest),matrix_as_list)
                                                                       )
                                                          ))), sep = "")
  }
  result <- paste(result,']',sep = "")
  result
}

generate_graphs <- function(results) {
  unlink('var_model_graphs', recursive = TRUE, force = TRUE)
  dir.create('var_model_graphs')
  setwd('var_model_graphs')
  for(result in results) {
    file_name <- result$name 
    file_name <- paste(strsplit(file_name, "\\.")[[1]][1], "json", sep=".")
    print(paste("Exporting: ", file_name, sep=""))
    
    # Write output to file
    sink(file_name)
    av_state <- list(accepted_models = list(list(varest = result$varest)))  
    cat(.convert_to_graph2(av_state,list(include_model = TRUE)))
    sink()
  }
  setwd("../")
}
