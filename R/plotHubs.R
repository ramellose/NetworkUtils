#' @title Generate SuperExactTest-compatible dataset
#' @description Takes list of hub species or highest centrality species and converts into SuperExactTest-compatible vectors
#' @details Includes all possible sets.
#'
#' @param list.hubs list of hubs per tool
#' @param toolnames list of toolnames
#' @param mode "hub" or "central": for hub, the degree is returned, for central, the centrality scores
#
#' @return Dataframe with hub or central species for tools
#' @export
plotHubs = function(hubs, toolnames,num){
  allvecs = list()
  for(j in 1:100){
    matset = list()
    hubvector = vector(mode="character")
    for (i in 1:length(toolnames)){
      subset = hubs[[i]]
      for (k in 1:num){
        species = subset[[j]][[k]]
        species = paste(as.character(j), species, sep="_")
        hubvector[k] = species
      }
      matset[[toolnames[i]]] = hubvector
    }
  allvecs[[j]] = matset
  }
  return(allvecs)
}


