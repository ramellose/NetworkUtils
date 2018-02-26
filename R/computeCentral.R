#' @title Compute centrality
#' @description Calculates betweenness centrality from matrix
#' @details Returns the top 5 highest betweenness centrality nodes
#'
#' @param matrix matrix with values, e.g. Spearman correlation or other tool output
#
#' @return top 5 betweenness centrality species
#' @export
computeCentral = function(matrix, num=5){
  graphmat = SpiecEasi::adj2igraph(matrix)
  central = list()
  for (i in 1:length(E(graphmat))){
    if (E(graphmat)$weight[i] == -1){
      E(graphmat)$weight[i] = 1
    }
  }
  between = igraph::betweenness(graphmat, v = V(graphmat), weights=NULL, directed=FALSE)
  if (max(between)!= 0){
    central = tail(sort(between),num)
  }
  central = names(central)
  return(central)
}
