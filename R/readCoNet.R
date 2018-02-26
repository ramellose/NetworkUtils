#' @title Read CoNet outputs
#' @description Reads CoNet .txt outputs and converts to adjacency matrix.
#' @details CoNet bash calls output a text file that can be used to compute adjacency matrices. This function parses the text file.
#' @param name name of p-value merge, "brown" or "fisher"
#' @param x vector specificying environmental strength or removed species
#' @param n number of replicates
#' @param alldata list of datasets with simulated counts
#' @param setname name of simulation
#' @param tool toolname, pref "CoNetBrown" or "CoNetFisher"
#' @param klemmadj list of Klemm matrices
#'
#' @return nothing, but writes .txt files to current directory
#' @export
readCoNet = function(name="brown", mode = "env", x=seq(from=0, to=50, by=5), n=10, alldata=alldata, setname, tool=tool, klemmadj, wdir, absolute=TRUE){
  results = list()
  for (i in 1:length(x)){
    resultssub = list()
    for (j in 1:n){
      if (name == "fisher"){
        if (mode == "hubs"){
          rstring = paste(j, "-fisher-hubs.txt", sep="")
        }
        else {
          rstring = paste(i, "_", j, "-fisher.txt", sep="")
        }
      }
      else if (name == "brown"){
        if (mode == "hubs"){
          rstring = paste(j, "-brown-hubs.txt", sep="")
        }
        else{
          rstring = paste(i, "_", j, "-brown.txt", sep="")
        }
      }
      txtdata = readLines(rstring)
      txspecies = c(rownames(alldata[[i]][[j]]))
      adjmatrix = matrix(nrow=length(txspecies),ncol=length(txspecies))
      colnames(adjmatrix) = txspecies
      rownames(adjmatrix) = txspecies
      adjmatrix = cbind(adjmatrix, rep(0, length(adjmatrix[,1])), rep(0, length(adjmatrix[,1])))
      adjmatrix = rbind(adjmatrix, rep(0, length(adjmatrix[1,])), rep(0, length(adjmatrix[1,])))
      colnames(adjmatrix)[c(101,102)] = c("factor1", "factor2")
      rownames(adjmatrix)[c(101,102)] = c("factor1", "factor2")
      specieslist = vector(mode="character")
      for (k in 3:length(txtdata)){
        species = stringr::str_sub(txtdata[k], start=-20)
        factors = unlist(regmatches(species, gregexpr("factor[[:digit:]]", species)))
        species = unlist(regmatches(species, gregexpr("sp[[:digit:]]+", species)))
        if (length(factors) == 1){
          index1 = which(rownames(adjmatrix) == factors[1])
          index2 = which(rownames(adjmatrix) == species[1])
        }
        else if (length(species) == 2) {
          index1 = which(rownames(adjmatrix) == species[1])
          index2 =  which(rownames(adjmatrix) == species[2])
        }
        adjmatrix[index1,index2] = 1
        adjmatrix[index2,index1] = 1
        if (absolute == FALSE){
          sign = stringr::str_sub(txtdata[k], end=50)
          if (grepl("mutualExclusion", sign)){
            adjmatrix[index1,index2] = -1
            adjmatrix[index2,index1] = -1
          }
        }
      }

      adjmatrix[is.na(adjmatrix)] = 0
      resultssub[[j]] = adjmatrix
    }
    results[[i]] = resultssub
  }
  setwd(wdir)
  saveRDS(results, paste("CoNet_", name, "_", setname, "_bashnetworks.rds", sep=""))
}
