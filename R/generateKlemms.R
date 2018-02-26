#' @title Generate Klemm adjacency matrices
#' @description Generates a list of Klemm-Eguiluz adjacency matrices
#' @details Generates a list compatible with other functions. For more details on Klemm-Eguiluz interaction matrices, please check the documentation of "generateA".
#'
#' @param n number of replicates
#' @param s number of species
#' @param pep positive edge percentage
#' @param c connectivity
#'
#' @return list of Klemm matrices
#' @export
generateKlemms = function(n, s, pep, c){
  klemms = list()
  klemmadj = list()
  for (i in 1:n){
    klemm = seqtime::generateA(s, "klemm", pep=10, c =0.05)
    klemms[[i]] = klemm
    klemmadj[[i]] = computeAdjacency(klemms[[i]])
  }
  return(list(klemms, klemmadj))
}
