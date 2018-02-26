#' @title Compute adjacency
#' @description Computes adjacency matrix with only 1, -1 and 0 values
#' @details Compute adjacency matrix from a correlation or covariance matrix with only significant edges
#'
#' @param matrix matrix with values, e.g. Spearman correlation or other tool output
#
#' @return Adjacency matrix
#' @export
computeAdjacency = function(matrix){
  matrix[row(matrix) == col(matrix)] = 0
  for (i in 1:length(matrix)){
    if (matrix[i] > 0) {
      matrix[i] = 1
    }
    else if (matrix[i] < 0){
      matrix[i] = -1
    }
  }
  return(matrix)
}
