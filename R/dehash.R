#' a virtual function, since one-way hashes only make sense forwards...
#' 
#' @param x     the thing to dehash
#' @param ...   arguments to pass to the called function for objects like x 
#' 
#' @return      an object of the same class as x, but with identities restored
#'
#' @export
dehash <- function(x, ...) {
  message("`dehash` is a purely virtual function stub for generic methods.")
}

# generic
setGeneric("dehash") 

# basic version
setMethod("dehash", "SummarizedExperiment", function(x, ...) dehashSE(x, ...))

# needs to include reducedDims and altExps
setMethod("dehash", "SingleCellExperiment", function(x, ...) dehashSCE(x, ...))

# needs to include assorted other stuff
setMethod("dehash", "BSseq", function(x, ...) dehashBSseq(x, ...))
