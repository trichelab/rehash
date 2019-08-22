#' deanonymize a rehashed MultiAssayExperiment and its constitutive objects 
#' 
#' Warning: this is not implemented properly yet. 
#' 
#' @param   x       a MultiAssayExperiment to deanonymize 
#' @param   meta    the meta-metadata required to reverse the hashes
#' @param   covs    a data.frame of covariates (rows are samples, columns covs)
#' @param   check   check assay row and column hashes? (FALSE; can be very slow)
#' 
#' @return          a dehashed MultiAssayExperiment
#' 
#' @export
dehashMAE <- function(x, meta=NULL, covs=NULL, check=FALSE) {
  stop("This function is not yet implemented (!)")
}
