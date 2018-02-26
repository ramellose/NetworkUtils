#' @title Generate SuperExactTest p-value vector corrected for sampling design
#' @description Takes list of hub species or highest centrality species and converts into p-values
#' @details Includes all possible sets.
#'
#' @param allvecs list of hubs per matrix / tool, plotHubs output
#
#' @return Vector with p-values that can be used to modify SuperExactTest object
#' @export
plotStats = function(allvecs){
  n = 100 # background population size
  allpvals = matrix(nrow=255, ncol=100)
  allfractions = matrix(nrow=255, ncol=100)
  for (i in 1:100){
    subtest = SuperExactTest::supertest(allvecs[[i]], n=100)
    allpvals[,i]= subtest$P.value
    allfractions[,i] = subtest$overlap.sizes
  }
  rownames(allpvals) = names(subtest$P.value)
  rownames(allfractions) = names(subtest$P.value)
  pvals = vector(mode="numeric")
  spvals = vector(mode="numeric")
  fractions = vector(mode="numeric")
  sfractions = vector(mode="numeric")
  for(i in 1:255){
    if (!is.na(allpvals[i,1])){
      pvals[i] = mean(allpvals[i,])
      spvals[i] = sd(allpvals[i,])
      fractions[i] = mean(allfractions[i,])
      sfractions[i] = sd(allfractions[i,])
    }
    else{
      pvals[i] = NA
      fractions[i] = NA
      spvals[i] = NA
      sfractions[i] = NA
    }
  }
  names(pvals) = rownames(allpvals)
  names(fractions) = rownames(allpvals)
  names(spvals) = rownames(allpvals)
  names(sfractions) = rownames(allpvals)
  return(list("pvals"=pvals, "spvals"=spvals, "fractions"=fractions, "sfractions"=sfractions))
}
