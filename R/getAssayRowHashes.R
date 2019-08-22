#' get row-wise digests for each assay in a SummarizedExperiment as a dataframe
#' 
#' @param x     a SummarizedExperiment-like object
#' @param algo  which algorithm to use (default "md5")
#' 
#' @return      a data.frame of row-wise assay hashes, one column per assay 
#' 
#' @import digest
#'
#' @export
getAssayRowHashes <- function(x, algo="md5") {
  res <- sapply(assays(x), .getRowHashes, algo=algo)
  rownames(res) <- rownames(x)
  return(res)
}

# helper fn
.getRowHashes <- function(x, algo="md5") {
  apply(stripDimnames(x), 1, digest, algo=algo)
}
