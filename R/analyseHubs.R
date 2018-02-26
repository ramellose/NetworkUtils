#' @title Return hubs or central species
#' @description Calculate the top 5 hub species or central species.
#' @details The top 5 hub species or betweenness centrality species are calculated for a list with including networks from different tools.
#'
#' @param networks list of list of networks per tool
#' @param toolnames list of toolnames
#' @param n number of replicates
#' @param mode "hub" or "central": for hub, the degree is returned, for central, the centrality scores
#
#' @return Dataframe specifying tool ID and degree
#' @export
analyseHubs = function(networks, toolnames, n, mode="hub", num=5){
  tool_hubs = list()
  for (i in 1:length(toolnames)){
    hubset = list()
    hubs = NA
    for (j in 1:n){
      if (mean(networks[[i]][[j]]) != 0){
        if (mode == "hub"){
          hubs = computeHub(networks[[i]][[j]], num)
        }
        if (mode == "central"){
          hubs = computeCentral(networks[[i]][[j]], num)
        }
      }
      set = list()
      if(typeof(hubs) == "character"){
        for (k in 1:length(hubs)){
          string = hubs[k]
          set[[k]] = string
        }
      }
      else {
        set = rep(NA, num)
      }
      hubset[[j]] = set
    }
    tool_hubs[[i]] = hubset
  }
  return(tool_hubs)
}
