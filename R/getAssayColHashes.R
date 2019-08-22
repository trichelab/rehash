#' get column-wise hashes for each assay in a SummarizedExperiment-like object
#' 
#' @param x     a SummarizedExperiment-like object
#' @param algo  which algorithm to use (default "md5")
#'
#' @return      a data.frame with column-wise hashes, one assay per column
#' 
#' @import digest
#'
#' @export
getAssayColHashes <- function(x, algo="md5") {
  res <- sapply(assays(x), .getColHashes, algo=algo)
  rownames(res) <- colnames(x)
  return(res)
}

# helper fn -- maybe put in utils?
.getColHashes <- function(x, algo="md5") {
  apply(stripDimnames(x), 2, digest, algo=algo)
}
