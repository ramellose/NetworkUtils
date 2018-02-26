#' @title Compute precision
#' @description Calculates precision of tools
#' @details Compares tool adjacency matrix output to Klemm-Eguiluz adjacency matrices
#'
#' @param imatrix true positive matrix, e.g. Klemm-Eguiluz adjacency matrix
#' @param outmatrix matrix with values, e.g. Spearman correlation or other tool output
#' @param absolute Calculates matches for absolute values if true instead of taking sign into account
#'
#' @return precision score
#' @export
computePrecision = function(imatrix, outmatrix, absolute = FALSE){
  outmatrix = t(outmatrix)
  poslist = which(imatrix != 0, arr.ind = T)
  p = length(poslist[,1])
  tp = 0
  for (i in 1:length(poslist[,1])){
    coords = poslist[i,]
    if (!absolute){
      if (imatrix[coords[1],coords[2]] == outmatrix[coords[1],coords[2]]){
        tp = tp + 1
      }
    }
    if (absolute){
      if (abs(imatrix[coords[1],coords[2]]) == abs(outmatrix[coords[1],coords[2]])){
        tp = tp + 1
      }
    }
  }
  neglist = which(imatrix == 0, arr.ind = T)
  n = length(neglist[,1])
  tn = 0
  for (i in 1:length(neglist[,1])){
    coords = neglist[i,]
    if (!absolute){
      if (imatrix[coords[1],coords[2]] == outmatrix[coords[1],coords[2]]){
        tn = tn + 1
      }
    }
    if (absolute){
      if (abs(imatrix[coords[1],coords[2]]) == abs(outmatrix[coords[1],coords[2]])){
        tn = tn + 1
      }
    }
  }
  fp = n - tn
  ppv = tp / (tp+fp)
  if (is.na(ppv)){
    ppv = 0
  }
  return(ppv)
}
