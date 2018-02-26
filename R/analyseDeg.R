#' @title Analyse degree or centrality
#' @description Analyses degree or centrality for a list of outputs from different tools
#' @details Outputs a dataframe with degrees for all tools.
#'
#' @param networks list of list of networks per tool
#' @param toolnames list of toolnames
#' @param n number of replicates
#' @param mode "hub" or "central": for hub, the degree is returned, for central, the centrality scores
#
#' @return dataframe specifying tool ID and degree
#' @export
analyseDeg = function(networks, toolnames, n, mode="hub"){
  degs = matrix(nrow=1, ncol=2)
  degs = data.frame
  tool_hubs = NULL
  for (i in 1:length(toolnames)){
    degree = matrix(nrow=10000, ncol=2)
    degree = data.frame(degree)
    degree[,1] = rep(toolnames[i], 10000)
    count = 1
    for (j in 1:n){
      set = networks[[i]][[j]]
      set = SpiecEasi::adj2igraph(set)
      if (mode == "hub"){
        out = igraph::degree(set)
      }
      if (mode == "central"){
        for (k in 1:length(E(set))){
          if (length(E(set)) != 0){
            if (E(set)$weight[k] == -1){
              E(set)$weight[k] = 1
            }
          }
        }
        out = betweenness(set, v = V(set), weights=NULL, directed=FALSE)
      }
      degree[(100*count-99):(100*count),2] = out
      count = count + 1
    }
    if (is.null(tool_hubs)){
      tool_hubs = degree
    }
    else {
      tool_hubs = rbind(tool_hubs, degree)
    }
  }
  tool_hubs$X1 = as.factor(tool_hubs$X1)
  levels(tool_hubs$X1)[8] = "True positives"
  return(tool_hubs)
}
