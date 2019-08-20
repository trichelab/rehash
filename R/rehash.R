#' a salty hashing function (and a generic that dispatches it)
#' 
#' @param x     the string to hash
#' @param salt  the salt to apply (NULL)
#' @param ...   other parameters to pass on to digest::digest
#' 
#' @return      an object of the appropriate class
#' 
#' @seealso dehash
#' @seealso rehashSE
#' @seealso rehashSCE
#' @seealso rehashBSseq 
#' 
#' @import digest
#' 
#' @export
rehash <- function(x, salt=NULL, ...) {
  if (length(x) > 1) sapply(x, rehash, salt=salt, ...)
  else return(digest::digest(paste0(salt, x), ...) ) 
}

# generics 
setGeneric("rehash")

# basic version
setMethod("rehash", "SummarizedExperiment", function(x, ...) rehashSE(x, ...))

# needs to include reducedDims and altExps (maybe?)
# setMethod("rehash", "SingleCellExperiment", function(x,...) rehashSCE(x,...))

# may need to include additional hooks here too 
# setMethod("rehash", "BSseq", function(x, ...) rehashBSseq(x, ...))

# may need to include a hook for annotation here, or just use defaults?
# setMethod("rehash", "GenomicRatioSet", function(x, ...) rehashGrSet(x, ...))
