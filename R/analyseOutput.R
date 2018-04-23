#' @title Compute network statistics
#' @description Compute multiple statistics for tool outputs.
#' @details Calculates precision, sensitivity, specificity, assortativity, degree, transitivity, AUC and scale-freeness for tool output.
#'
#' @param network list of networks for one tool
#' @param klemmadj list of Klemm matrices
#' @param tool toolname
#' @param setname name of simulation
#' @param x vector specificying environmental strength or removed species
#' @param n number of replicates
#' @param absolute if true: only check for edge presence, not matching sign
#' @param mode "env" or "abundance", env takes environmental strength into account while "abundance" includes species removal
#' @param alldata list of datasets with simulated counts
#
#' @return List of dataframes with results per point of x
#' @export
analyseOutput = function(network, klemmadj, tool, setname, x, n, absolute = TRUE, alldata=NULL){
  scores = list()
  for (i in 1:length(x)){
    output = matrix(nrow=n, ncol=8)
    output = data.frame(output)
    colnames(output) = c("Precision", "Sensitivity", "Specificity",
                         "Assortativity", "Degree", "Transitivity", "AUC", "Modularity")
    subset = network[[i]]
    for (j in 1:n){
      species = rownames(alldata[[i]][[j]])
      species = as.numeric(gsub("sp", "", species))
      mat = klemmadj[[j]][species, species]
      output[j,c(1,2,3,4,5,6,7,8)] = 0
      mean = mean(subset[[j]])
      if (mean != 0 && !is.na(mean)){
        output[j,1] = computePrecision(mat, subset[[j]], absolute)
        output[j,2] = computeSensitivity(mat, subset[[j]], absolute)
        output[j,3] = computeSpecificity(mat, subset[[j]], absolute)
        output[j,4] = computeStat(subset[[j]], mode="assortativity")
        output[j,5] = mean(computeStat(subset[[j]], mode="degree"))
        output[j,6] = computeStat(subset[[j]], mode="transitivity")
        #output[j,7] = computeAUC(mat, subset[[j]])
        #output[j,8] = computeStat(subset[[j]], mode="modularity")
      }
      if (mean == 0 ){
        output[j,c(1:8)] = NA
      }
      if (is.na(mean)){
        output[j,c(1:8)] = NA
      }
    }
    scores[[i]] = output
  }
  results = list(scores)
  text = paste(tool, setname, ".rds", sep="")
  saveRDS(results, text)
  return(results)
}
