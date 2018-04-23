#' @title Write features
#' @description Writes features to txt files.
#' @details List of features can be written to text files for use by CoNet bash calls.
#'
#' @param n number of replicates
#' @param x vector specificying environmental strength or removed species
#' @param dataset full dataset
#'
#' @return nothing, but writes .txt files to current directory
#' @export
writeFeatures = function(n, x, edges){
  conditions = list()
  factor1 = vector(mode="numeric")
  factor2 = vector(mode="numeric")
  factor1[1:40] = 0
  factor1[41:80] = 0
  factor2[1:40] = 0
  factor2[41:80] = 0
  features = data.frame(factor1, factor2)
  features = t(features)
  for (j in 1:n){
    string = paste(1,"_",j, "-features.txt", sep="")
    write.table(features, string, sep="\t", col.names=NA)
  }
  for (i in 2:length(x)){
    set = list()
    for (j in 1:n){
      condition1 = edges[[i]][[1]][[1]][1,]
      condition2 = edges[[i]][[1]][[1]][2,]
      factor1 = vector(mode="numeric")
      factor2 = vector(mode="numeric")
      factor1[1:40] = condition1[1]
      factor1[41:80] = condition2[1]
      factor2[1:40] = condition1[2]
      factor2[41:80] = condition2[2]
      features = data.frame(factor1, factor2)
      features = t(features)
      set[[j]] = features
      string = paste(i,"_",j, "-features.txt", sep="")
      write.table(features, string, sep="\t", col.names=NA)
    }
    conditions[[i]] = set
  }
  saveRDS(conditions, "features.rds")
}
