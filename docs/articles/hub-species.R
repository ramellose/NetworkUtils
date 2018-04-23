## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(NetworkUtils)

## ---- eval = FALSE-------------------------------------------------------
#  setname = "_hub"
#  wdir="insertfilepath" # Needs to be supplied for the other sections of code to restore your filepath
#  setwd(wdir)
#  
#  allhubklemm = NetworkUtils::generateKlemms(100, 100, 10, 0.05) # the generateKlemms function has two outputs:
#  hubklemms = allklemm[[1]] # a matrix with interaction strengths, used to generate datasets
#  hubklemmadj=allklemm[[2]] # an adjacency matrix, used to evaluate inferred networks
#  
#  # This function generates the datasets
#  hubdata = generateSets(n=100, klemms=hubklemms, species=100, samples=80, x=1, mode="env", name=setname)
#  
#  # We ran CoNet and SparCC from a VM; make sure you set the working directory correctly before using writeSets or writeFeatures
#  writeSets(n=100, x=1, hubdata)

## ---- eval = FALSE-------------------------------------------------------
#  # This function runs network inference and analyses the inferred networks
#  callTools(hubdata, hubklemmadj, toolnames=c("SpiecEasi GL", "SpiecEasi MB", "gCoda", "Spearman"), setname, x=1, n=100, absolute = TRUE,  mode="env")
#  
#  # After running bashscript_CoNet.bash, this function can be used to parse the CoNet output
#  readCoNet(name="brown", mode="hubs", x=1, n=100, alldata=hubdata, setname=set4, tool="CoNet Brown", klemmadj=hubklemmadj, wdir=wdir)
#  readCoNet(name="fisher", mode="hubs", x=1, n=100, alldata=hubdata, setname=set4, tool="CoNet Fisher", klemmadj=hubklemmadj, wdir=wdir)
#  readSpar(n=100, x=1, mode="hubs", alldata=hubdata, setname=set4, klemmadj=hubklemmadj, wdir=wdir)
#  

