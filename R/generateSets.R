#' @title Generate multiple datasets
#' @description Generate a list of list of datasets with n replicates for x datapoints.
#' @details Calls on the envGrowthChanges, generateDataSet and glv functions from seqtime to generate datasets compatible with other functions.
#'
#' @param x vector specificying environmental strength or removed species
#' @param n number of replicates
#' @param mode "env" or "abundance", env takes environmental strength into account while "abundance" includes species removal
#' @param name filename of output dataset
#'
#' @return Returns nothing, but saves .rds files of datasets
#' @export
generateSets = function(n, klemms, species, samples, x, mode="env", name){
  alldata = list()
  envlist = list()
  for (i in 1:length(x)){
    data = list()
    if (mode == "env"){
      env = list()
      for (j in 1:n){
        env1 = NetworkUtils::envGrowthChanges(species=species,  env.factors=2, conditions=2, strength=x[i])
        env[[j]] = env1
        subset = seqtime:::generateDataSet(samples, klemms[[j]], env.matrix = env1[[2]], perturb.count = c(40,40))
        data[[j]] = subset
      }
      envlist[[i]] = env
      saveRDS(envlist, "environmentalfactors+species_edges.rds")
    }
    else {
      for (j in 1:n){
        subset = seqtime:::generateDataSet(80, klemms[[j]])
        data[[j]] = subset
      }
    }
    alldata[[i]] = data
  }
  if (mode == "abundance"){
    for (i in 2:length(x)){
      for (j in 1:n){
        removeN = x[i]
        alldata[[i]][[j]] = seqtime:::removeLowAbundance(alldata[[i]][[j]], removeN)
      }
    }
  }
  saveRDS(alldata, paste(name, ".rds", sep=""))
  return(alldata)
}
