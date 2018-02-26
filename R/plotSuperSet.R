#' @title Generate ggplots using some SuperExactTest code
#' @description Takes list of hub species or highest centrality species and outputs heatmap
#' @details Includes all possible sets.
#' Internally, this function makes calls to PlotStats; this function replaces statistics for the entire dataset
#' with p-values per individual dataset. Otherwise p-values are much too inflated.
#'
#' @param list.hubs list of hubs per tool
#' @param toolnames list of toolnames
#' @param mode "hub" or "central": for hub, the degree is returned, for central, the centrality scores
#
#' @return Dataframe with hub or central species for tools
#' @export
plotSuperSet = function(hubs, toolnames, num, plot=TRUE){
  allvecs = list()
  for(i in 1:length(toolnames)){
    subset = hubs[[i]]
    hubvector = vector(mode="character")
    count = 1
    for(j in 1:100){
      for (k in 1:num){
        species = subset[[j]][[k]]
        species = paste(as.character(j), species, sep="_")
        hubvector[count] = species
        count = count + 1
      }
    }
    allvecs[[toolnames[[i]]]] = hubvector
  }
  total = 100*100
  subtotal = 100
  superhub=SuperExactTest::supertest(allvecs, n=total)
  statset = plotHubs(hubs, tools, num)
  stats = plotStats(statset)
  superhub$P.value = stats$pvals
  superhub$overlap.sizes = stats$fractions
  setsizes = matrix(nrow=255, ncol=6)
  setsizes[,1] = superhub$overlap.sizes
  setsizes[,2] = names(superhub$overlap.sizes)
  setsizes[,3] = sapply(setsizes[,2], function(x) lengths(regmatches(x, gregexpr("1", x))))
  setsizes[,4] = superhub$P.value
  setsizes[,5] = stats$spvals
  setsizes[,6] = stats$sfractions
  setsizes = setsizes[setsizes[,3] == 2,]
  sizemat = matrix(nrow=8, ncol=8)
  diag(sizemat) = 5
  pvals = sizemat
  diag(pvals) = 0
  spvals = sizemat
  sfractions = sizemat
  for (i in 1:length(setsizes[,2])){
    pos = gregexpr(pattern ='1', setsizes[i,2])
    pos1 = pos[[1]][1]
    pos2 = pos[[1]][2]
    sizemat[pos1, pos2] = as.numeric(as.character(setsizes[i,1]))
    pvals[pos1, pos2] = as.numeric(as.character(setsizes[i,4]))
    spvals[pos1, pos2] = as.numeric(as.character(setsizes[i,5]))
    sfractions[pos1, pos2] = as.numeric(as.character(setsizes[i,6]))
  }
  sizemat[lower.tri(sizemat)] = t(sizemat)[lower.tri(sizemat)]
  pvals[lower.tri(pvals)] = t(pvals)[lower.tri(pvals)]
  spvals[lower.tri(spvals)] = t(spvals)[lower.tri(spvals)]
  sfractions[lower.tri(sfractions)] = t(sfractions)[lower.tri(sfractions)]
  colnames(sizemat) = superhub$set.names
  rownames(sizemat) = superhub$set.names
  colnames(pvals) = superhub$set.names
  rownames(pvals) = superhub$set.names
  colnames(spvals) = superhub$set.names
  rownames(spvals) = superhub$set.names
  colnames(sfractions) = superhub$set.names
  rownames(sfractions) = superhub$set.names
  sizes = reshape2::melt(sizemat)
  pvals = reshape2::melt(pvals)
  spvals = reshape2::melt(spvals)
  sfractions = reshape2::melt(sfractions)
  pvals$value[pvals$value == Inf] = NA
  sizes$value[sizes$value == Inf] = NA
  spvals$value[spvals$value == Inf] = NA
  sfractions$value[sfractions$value == Inf] = NA
  if (plot){
    plotsize = ggplot(data=sizes, aes(x=sizes$Var1,y=sizes$Var2)) + geom_tile(aes(fill=sizes$value)) +
      labs(x="", y="", fill="Mean \nmatches \n") + viridis::scale_fill_viridis() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "white"))
    pvals$value[pvals$value > 0.5] = NA
    plotp = ggplot(data=pvals, aes(x=pvals$Var1,y=pvals$Var2)) + geom_tile(aes(fill=pvals$value)) +
      labs(x="", y="", fill="Mean \np-value \n") + viridis::scale_fill_viridis(direction=-1) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "white"))
    return(list(plotsize, plotp))
  }
  else{
    return(list("fractions"=sizes, "pvals"=pvals, "sdfractions"=sfractions, "sdpvals"=spvals))
  }
}

