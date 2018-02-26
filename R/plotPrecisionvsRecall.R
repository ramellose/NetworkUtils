#' @title Plot precision vs recall
#' @description Produces ggplot with precision vs sensitivity scatterplot, coloured by tool.
#' @details Requires the output statistics file, not a network file.
#'
#' @param results output statistics per tool
#' @param toolnames list of toolnames
#' @param x vector specificying environmental strength or removed species
#' @param colours vector specificying codes in hex
#' @return ggplot object
#' @export
plotPrecisionvsRecall = function(results, toolnames, x, colours, mode="env"){
  framelist = list()
  for (k in 1:length(results)){
    set = results[[k]]
    frame = set[[1]][[1]]
    frame = as.data.frame(frame)
    frame = cbind(frame, rep(x[1],10))
    colnames(frame)[8]="strength"
    if (mode == "env"|mode=="abn"){
      for (i in 2:length(x)){
        newframe = set[[1]][[i]]
        newframe = cbind(newframe, rep(x[i],10))
        colnames(newframe) = colnames(frame)
        frame = rbind(frame,newframe)
      }
    }

    frame = frame[,c(1,2,9)]
    frame = cbind(rep(toolnames[k], length(frame[,1])), frame)
    framelist[[k]] = frame
  }
  frames = framelist[[1]]
  for (i in 2:length(framelist)){
    frames = rbind(frames, framelist[[i]])
  }
  colnames(frames) = c("Tool", "Precision", "Sensitivity", "Strength")
  frames$Precision = as.numeric(as.character(frames$Precision))
  frames$Sensitivity = as.numeric(as.character(frames$Sensitivity))
  frames$Strength = as.numeric(as.character(frames$Strength))
  if (mode == "env"){
    plot1 = ggplot(data=frames, aes(x=frames$Precision, y=frames$Sensitivity, color=frames$Tool, size=frames$Strength)) +
      geom_point(alpha=4/10) +
      labs(color="Tools", y="Sensitivity", x="Precision", size="Strength") + scale_color_manual(name="Tools", values = colours) +
      scale_size_continuous(range=c(2,5)) + theme_minimal()+ theme(legend.key.size=unit(0.3,"cm"))
  }
  if (mode == "abn"){
    plot1 = ggplot(data=frames, aes(x=frames$Precision, y=frames$Sensitivity, color=frames$Tool, size=frames$Strength)) +
      geom_point(alpha=4/10) +
      labs(color="Tools", y="Sensitivity", x="Precision", size="Removed\nspecies")+ scale_color_manual(name="Tools", values = colours) +
      scale_size_continuous(range=c(2,5)) + theme_minimal()+ theme(legend.key.size=unit(0.3,"cm"))
  }
  if (mode == "hub"){
    plot1 = ggplot(data=frames, aes(x=frames$Precision, y=frames$Sensitivity, color=frames$Tool)) +
      geom_point(alpha=4/10) +
      labs(color="Tools", y="Sensitivity", x="Precision") + scale_color_manual(name="Tools", values = colours) +
      scale_size_continuous(range=c(2,5)) + theme_minimal()+ theme(legend.key.size=unit(0.3,"cm"))
  }
  return(plot1)
}
