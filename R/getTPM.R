#' Get transcripts per million (TPM) from a recount RangedSummarizedExperiment
#'
#' @param object  an RSE from recount 
#' @param ...     any additional arguments to pass to recount::getRPKM
#'
#' @return        a matrix of TPM estimates
#'
#' @seealso download_study
#' @seealso rse_gene_SRP009615
#' 
#' @examples
#' library(recount)
#' getTPM(rse_gene_SRP009615)
#'
#' @import recount
#' 
#' @export 
getTPM <- function(object, ...) {
  RPKMtoTPM(recount::getRPKM(object, ...))
}
