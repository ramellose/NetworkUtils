#' @title Compute AUC
#' @description Calculates AUC coefficient from matrix
#' @details Wrapper for pROC function that converts matrix to igraph object
#'
#' @param matrix matrix with values, e.g. Spearman correlation or other tool output
#
#' @return AUC score
#' @export
computeAUC = function(imatrix, outmatrix){
  imatrix = as.vector(imatrix)
  outmatrix = as.vector(outmatrix)
  for (i in 1:length(imatrix)){
    if (imatrix[i] == -1){
      imatrix[i] = 1
    }
  }
  for (i in 1:length(outmatrix)){
    if (outmatrix[i] == -1){
      outmatrix[i] = 1
    }
  }
  imatrix = as.numeric(imatrix)
  outmatrix = as.numeric(outmatrix)
  auc = pROC::auc(outmatrix, imatrix)
  return(auc)
}

computeScaleFree = function(matrix){
  graph = SpiecEasi::adj2igraph(matrix)
  deg = degree(graph)
  sf = igraph::fit_power_law(deg)
  sf = sf$KS.stat
  if (sf > 5){
    sf = NaN
  }
  return(sf)
}
