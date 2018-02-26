#' @title Analyse Klemm matrices
#' @description Outputs network statistics for a list of Klemm-Eguiluz matrices.
#' @details Outputs assortativity, degree, transitivity and scale-freeness for Klemm-Eguiluz true positive matrices.
#'
#' @param klemmadj list of Klemm matrices
#' @param setname name of simulation
#' @param n number of replicates
#
#' @return Dataframe with statistics for Klemm-Eguiluz matrices
#' @export
analyseKlemm = function(klemmadj, n){
  output = matrix(nrow=n, ncol=5)
  output = data.frame(output)
  colnames(output) = c("Assortativity", "Degree", "Transitivity", "Modularity")
  for (j in 1:n){
    output[j,1] = computeStat(klemmadj[[j]], mode="assortativity")
    output[j,2] = mean(computeStat(klemmadj[[j]], mode="degree"))
    output[j,3] = computeStat(klemmadj[[j]], mode="transitivity")
    output[j,4] = computeStat(klemmadj[[j]], mode="modularity")
  }
  output[,5] = rep(0, n)
  text = paste("klemmspecs.rds", sep="")
  saveRDS(output, text)
  return(output)
}
