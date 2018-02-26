#' @title Plot statistics
#' @description Produces ggplot with specified statistic, coloured by tool.
#' @details Requires the output statistics file, not a network file. Statistics that can be plotted are:
#' "Precision", "Sensitivity", "Specificity", "AUC", "Transitivity", "Degree", "Scale-freeness"
#'
#' @param results output statistics per tool
#' @param klemmspecs output statistics for Klemm-Eguiluz matrices
#' @param toolnames list of toolnames
#' @param x vector specificying environmental strength or removed species
#' @param statistic statistic to be plotted
#' @param axis x axis title
#' @param mode "linear" or "quadratic" for trend line
#' @param legend if TRUE, show legend
#' @param colours vector specificying codes in hex
#' @return ggplot object
#' @export
plotSeries = function(results, klemmspecs, toolnames, x, statistic, axis, mode="linear", legend=TRUE, colours){
  framelist = list()
  for (k in 1:length(results)){
    set = results[[k]]
    frame = set[[1]][[1]]
    frame = as.matrix(as.data.frame(frame))
    frame = cbind(frame, rep(x[1], 5))
    colnames(frame)[9] = axis
    for (i in 2:length(x)){
      newframe = set[[1]][[i]]
      newframe = cbind(newframe, rep(x[i], 5))
      colnames(newframe) = colnames(frame)
      frame = rbind(frame,newframe)
    }
    frame = reshape2::melt(frame, id.vars = axis)
    framelist[[k]] = frame
  }
  newframe = framelist[[1]]
  newframe = newframe[newframe$variable == statistic,]
  L = length(newframe[,1])
  newframe$tool = rep(toolnames[1], L)
  for (i in 2:length(results)){
    new = framelist[[i]]
    new = new[new$variable == statistic,]
    L = length(new[,1])
    new$tool = rep(toolnames[i], L)
    newframe = rbind(newframe,new)
  }
  # ggplot
  plot = ggplot(data = newframe, aes(x=newframe[,1], y=value, color=tool)) +
    geom_point(shape=1, size=3, stroke=1.2) + labs(color="Tools") +  xlab(axis) +ylab(statistic) + scale_color_manual(values = colours)
  if (legend == FALSE){
    plot = plot + theme(legend.position="none")
  }

  if (mode == "linear"){
    plot = plot + geom_smooth(method="lm")
  }
  if (mode =="quadratic"){
    plot = plot + stat_smooth(method="lm", formula = y ~ x + I(x^2), size=1)
  }
  if (statistic == "Assortativity"){
    plot = plot + geom_point(data = klemmspecs, aes(x=klemmspecs[,5], y=klemmspecs[,1], color="True positives"), size=4)
  }
  if (statistic == "Degree"){
    plot = plot + geom_point(data = klemmspecs, aes(x=klemmspecs[,5], y=klemmspecs[,2], color="True positives"), size=4)
  }
  if (statistic == "Transitivity"){
    plot = plot + geom_point(data = klemmspecs, aes(x=klemmspecs[,5], y=klemmspecs[,3], color="True positives"), size=4)
  }
  if (statistic == "Modularity"){
    plot = plot + geom_point(data = klemmspecs, aes(x=klemmspecs[,5], y=klemmspecs[,4], color="True positives"), size=4)
  }
  if (statistic == "Precision" | statistic == "Sensitivity" | statistic == "Specificity" | statistic == "AUC"){
    plot = plot + coord_cartesian(ylim = c(0,1))
  }
  return(plot)
}
