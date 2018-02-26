#' @title Call tools to infer networks
#' @description Infer networks using SpiecEasi, Spearman or gCoda
#' @details Calls on original implementations of SpiecEasi and gCoda or the CoNet implementation of Spearman. The wrapper also ensures that adjacency matrices are returned that are compatible with other functions.
#'
#' @param alldata list of datasets with simulated counts
#' @param klemmadj list of Klemm matrices
#' @param toolnames list of toolnames
#' @param setname name of simulation
#' @param x vector specificying environmental strength or removed species
#' @param n number of replicates
#' @param absolute if true: only check for edge presence, not matching sign
#' @param mode "env" or "abundance", env takes environmental strength into account while "abundance" includes species removal
#
#' @return Returns nothing, but saves .rds files of networks and statistics
#' @export
callTools = function(alldata, klemmadj, toolnames, setname, x, n, absolute = FALSE, mode="env"){
  for (i in 1:length(toolnames)){
    toolset = list()
    toolname = toolnames[i]
    string = paste(toolname, setname, "networks.rds", sep="")
    num = 1
    for (j in 1:length(x)){
      subset = list()
      data = alldata[[j]]
      for (k in 1:n){
        dataset = data[[k]]
        if (toolname == "SpiecEasi GL"){
          spiec.out = testSpiec(dataset, method="glasso")
          spiec.adj = computeAdjacency(spiec.out)
          subset[[k]] = spiec.adj
        }
        if (toolname == "SpiecEasi MB"){
          spiec.out = testSpiec(dataset, method="mb")
          spiec.adj = computeAdjacency(spiec.out)
          subset[[k]] = spiec.adj
        }
        if (toolname == "Spearman"){
          spear.out = testSpear(dataset)
          spear.adj = computeAdjacency(spear.out)
          subset[[k]] = spear.adj
        }
        if (toolname == "gCoda"){
          coda.out = testgCoda(dataset)
          subset[[k]] = coda.out
        }
        print(paste("Finished dataset:", num))
        num = num + 1
      }
      toolset[[j]] = subset
      saveRDS(toolset, string)
    }
    if (mode == "env"){
      analyseOutput(toolset, klemmadj, toolname, setname, x=x, n=n, absolute, alldata=alldata)
    }
    else if (mode == "abundance"){
      analyseOutput(toolset, klemmadj, toolname, setname, x=x, n=n, absolute, alldata=alldata)
    }
    wrap = paste(toolname, " finished!", sep="")
    print(wrap)
  }
}
