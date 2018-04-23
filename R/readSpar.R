#' @title Read SparCC outputs
#' @description Reads SparCC .txt outputs and converts to adjacency matrix.
#' @details SparCC bash calls output p-values that can be used to compute adjacency matrices.
#'
#' @param n number of replicates
#' @param setname name of simulation
#' @param x vector specificying environmental strength or removed species
#' @param alldata list of datasets with simulated counts
#' @param klemmadj list of Klemm matrices
#'
#' @return Nothing, but writes .txt files to current directory
#' @export
readSpar = function(n, x, mode=NA, setname, alldata, klemmadj, wdir){
  set = list()
  for (i in 1:length(x)){
    setlist = list()
    for (j in 1:n){
      if (mode == "hubs"){
        string = paste(j, "-pvals.txt", sep="")

      }
      else{
        string = paste(i, "_", j, "-pvals.txt", sep="")
      }
      mat = read.table(string)
      # compute adjacency for p < 0.05
      mat2 = as.matrix(mat)
      mat2[mat < 0.001] = 1
      mat2[mat >= 0.001] = 0
      setlist[[j]] = mat2
    }
    set[[i]] = setlist
  }
  setwd(wdir)
  saveRDS(set, paste("SparCC", setname, "_bashnetworks.rds"))
  results = analyseOutput(set, klemmadj, tool="SparCC", setname=setname, x=x, n=n, alldata=alldata, absolute=TRUE)
}
