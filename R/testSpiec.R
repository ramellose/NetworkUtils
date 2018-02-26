#' @title Calls SpiecEasi
#' @description Calls SpiecEasi and formats output to adjacency matrix
#' @details Authors: Zachary D. Kurtz et al. SpiecEasi is available at: https://github.com/zdk123/SpiecEasi
#'
#' @param data input dataset
#' @param method "mb" or "glasso", please check SpiecEasi's documentation for more information
#' @return adjacency matrix inferred from data
#' @export
testSpiec = function(data, method="mb"){
  data = t(data)
  spiec.out = SpiecEasi::spiec.easi(data, method, icov.select.params=list(rep.num=20))
  adj = as.matrix(spiec.out$refit)
  # Process Spiec-Easi graph to compare to Klemm matrix
  if (method == "mb"){
    # betaMat only works for MB method, cannot extract signs with glasso
    adj = as.matrix(SpiecEasi::getOptBeta(spiec.out))
  }
  # need to convert from igraph again to get non-sparse matrix
  return(adj)
}
