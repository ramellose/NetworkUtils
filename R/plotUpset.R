#' @title Generate UpSetplot-compatible dataset
#' @description Takes list of hub species or highest centrality species and converts into upset-compatible plot.
#'
#' @param list.hubs list of hubs per tool
#' @param toolnames list of toolnames
#
#' @return Dataframe with hub or central species for tools
#' @export
plotUpset = function(list.hubs, list.toolnames){
  frame = matrix(nrow=1, ncol=3)
  frame = data.frame(frame)
  colnames(frame) = c("matrix", "Tool", "ID")
  count = 1
  for (i in 1:length(list.hubs)){
    for (j in 1:length(list.hubs[[i]])){
      matresult = list.hubs[[i]][[j]]
      for (k in 1:length(matresult)){
        frame[count,1] = j
        frame[count,2] = list.toolnames[i]
        frame[count,3] = matresult[[k]]
        count = count + 1
      }
    }
  }
  frame$matrixID = paste(frame$matrix, frame$ID)
  ids = unique(frame$matrixID)
  upsetframe = matrix(nrow=length(list.toolnames), ncol=length(ids))
  upsetframe - data.frame(upsetframe)
  rownames(upsetframe) = list.toolnames
  for (i in 1:length(ids)){
    upsetframe[,i] = NA
    for (j in 1:length(list.toolnames)){
      subset = frame[frame$Tool == list.toolnames[j],]
      if (ids[i] %in% subset$matrixID){
        upsetframe[j,i] = 1
      }
      else {
        upsetframe[j,i] = 0
      }
    }
  }
  colnames(upsetframe) = ids
  upsetframe = t(upsetframe)
  upsetframe = data.frame(upsetframe)
  colnames(upsetframe) = c(list.toolnames)
  return(upsetframe)
}
