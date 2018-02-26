#' @title Compute hubs
#' @description Identifies top 5 hub species from matrix
#' @details Returns the top 5 highest degree species
#'
#' @param matrix matrix with values, e.g. Spearman correlation or other tool output
#
#' @return top 5 hub species
#' @export
computeHub = function(matrix, num=5){
  graphmat = SpiecEasi::adj2igraph(matrix)
  deg = igraph::degree(graphmat, v = V(graphmat), mode = c("all"), loops = FALSE)
  hubs = list()
  if (max(deg)!= 0){
    hubs = tail(sort(deg),num)
  }
  hubs = names(hubs)
  return(hubs)
}
