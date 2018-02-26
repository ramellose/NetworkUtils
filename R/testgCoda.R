#' @title Calls gCoda function
#' @description Calls gCoda and formats output to adjacency matrix
#' @details Author: Fang Huaying. gCoda is available at: https://github.com/huayingfang/gCoda
#'
#' @param data input dataset
#
#' @return adjacency matrix inferred from data
#' @export
testgCoda = function(data){
  N = length(data[,1])
  data = t(data)
  adjmatrix = gcoda(data, counts=T)
  adjmatrix = adjmatrix$refit
  return(adjmatrix)
}

################################################################################
# File: gcoda.R
# Aim : Conditional dependence network inference for compositional data
#-------------------------------------------------------------------------------
# Author: Fang Huaying (Peking University)
# Email : hyfang@pku.edu.cn
# Date  : 13MAY2016
#-------------------------------------------------------------------------------

