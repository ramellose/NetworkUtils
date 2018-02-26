#' @title Compute assortativity, transitivity or degree
#' @description Calculates assortativity coefficient from matrix
#' @details Wrapper for igraph function that converts matrix to igraph object
#'
#' @param matrix matrix with values, e.g. Spearman correlation or other tool output
#
#' @return assortativity score
#' @export
computeStat = function(matrix, mode){
  graph = SpiecEasi::adj2igraph(matrix)
  score = assortativity.degree(graph)
  if (mode == "transitivity"){
    score = transitivity(graph, type=c("global"))
  }
  else if (mode == "degree"){
    score = igraph::degree(graph)
  }
  else if (mode == "assortativity"){
    score = assortativity.degree(graph)
  }
  else if (mode == "modularity"){
    groups = igraph::cluster_walktrap(graph)
    score = igraph::modularity(graph, membership(groups))
  }
  return(score)
}
