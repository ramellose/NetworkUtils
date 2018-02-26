#' @title Calls Spearman correlation
#' @description Calls Spearman correlation as implemented in CoNet and formats output to adjacency matrix
#' @details Author: Karoline Faust.
#' Note that the CoNetinR implementation simply makes calls to the stat cor and cor.test functions.
#' Implementation in CoNetinR is therefore convenience only, as graphs are in the same format.
#'
#' @param data input dataset
#
#' @return adjacency matrix inferred from data
#' @export
testSpear = function(data){
  N = length(data[,1])
  adjmatrix = matrix(0, nrow=N, ncol=N)
  scores = CoNetinR::getNetwork(mat = data, method="spearman", T.up=0.2, T.down=-0.2, shuffle.samples=F, norm=TRUE, rarefy=0, stand.rows=F, pval.cor=F, permut=F, renorm=F, permutandboot=F, iters=100, bh=T, min.occ=0, keep.filtered=F,report.full=T, verbose=F)
  scores = scores$scores
  pmatrix = CoNetinR::getNetwork(mat = data, method="spearman", T.up=0.2, T.down=-0.2, shuffle.samples=F, norm=TRUE, rarefy=0, stand.rows=F, pval.cor=T, permut=F, renorm=F, permutandboot=F, iters=100, bh=T, min.occ=0, keep.filtered=F, report.full=T, verbose=F)
  pmatrix = pmatrix$pvalues
  adjmatrix = matrix(nrow = N, ncol = N)
  adjmatrix[lower.tri(adjmatrix)] = scores
  adjmatrix = t(adjmatrix)
  adjmatrix[lower.tri(adjmatrix)] = scores
  for (i in 1:N){
    for (j in 1:N){
      if (is.na(adjmatrix[i,j])){
        adjmatrix[i,j] = 0
      }
      else if (pmatrix[i,j] > 0.05){
        adjmatrix[i,j] = 0
      }
    }
  }
  return(adjmatrix)
}
