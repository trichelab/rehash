#' a virtual function, since one-way hashes only make sense forwards... thus
#' delegates (dehashSE for SummarizedExperiments, dehashSCE for 
#' SingleCellExperiments, dehashBSseq for BSseq objects, and so forth) do all 
#' of the heavy lifting for this method.
#' 
#' @param x     the thing to dehash
#' @param ...   arguments to pass to the called function for objects like x 
#' 
#' @return      an object of the same class as x, but with identities restored
#' 
#' @seealso rehash
#' @seealso dehashSE
#' @seealso dehashSCE
#' @seealso dehashBSseq 
#'
#' @import minfi
#' @import bsseq
#' @import SummarizedExperiment
#' @import SingleCellExperiment
#'
#' @export
dehash <- function(x, ...) {
  message("`dehash` is a purely virtual function stub for generic methods.")
}

# generic
setGeneric("dehash") 

# basic version
setMethod("dehash", "SummarizedExperiment", function(x, ...) dehashSE(x, ...))

# needs to include reducedDims and altExps?
# setMethod("dehash", "SingleCellExperiment", function(x,...) dehashSCE(x,...))

# needs to include assorted other stuff? (maybe)
# setMethod("dehash", "BSseq", function(x, ...) dehashBSseq(x, ...))

# needs to include assorted other stuff? (maybe not!)
# setMethod("dehash", "GenomicRatioSet", function(x, ...) dehashGrSet(x, ...))
