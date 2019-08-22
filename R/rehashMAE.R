#' anonymize (to some degree) a MultiAssayExperiment and its constituent objects
#'
#' Warning: this function is not yet properly implemented. 
#' 
#' @param   x           a MultiAssayExperiment to anonymize 
#' @param   salt        a salting phrase to slow brute-force attacks ("0x")
#' @param   strip       strip rehashed objects of any deID'ing metadata? (TRUE)
#' @param   algo        algorithm to use for the one-way hash (default is "md5")
#' @param   deorder     scramble rows and columns? (FALSE; disrupts data digest)
#' 
#' @return              a new, rehashed MultiAssayExperiment
#' 
#' @import  MultiAssayExperiment
#' @import  digest
#' 
#' @export
rehashMAE <- function(x, salt="0x", strip=TRUE, algo="md5", deorder=FALSE) { 
  stop("This function is not yet implemented (!)")
}
