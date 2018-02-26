#' @title Write datasets
#' @description Writes data sets to txt files.
#' @details List of datasets can be written to text files for use by SparCC or CoNet bash calls.
#'
#' @param n number of replicates
#' @param x vector specificying environmental strength or removed species
#' @param dataset full dataset
#'
#' @return nothing, but writes .txt files to current directory
#' @export
writeSets = function(n, x, dataset){
  for (i in 1:length(x)){
    for (j in 1:n){
      set = dataset[[i]][[j]]
      string = paste(i,"_",j, ".txt", sep="")
      write.table(set, string, sep="\t", col.names=NA)
    }
  }
}
