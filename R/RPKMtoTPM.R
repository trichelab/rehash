#' Reads per kilobase per million (RPKM) to transcripts per million (TPM)
#'
#' Per Colin Dewey, TPM == (RPKM[j] / sum(RPKM)) * 1000000, for every feature j.
#' (The same is true of FPKM, since RPKM just estimates fragments of template.)
#'
#' @param object  a (DelayedM|m)atrix of RPKM/FPKMs, or something containing one
#' @param ...     any additional user input arguments to pass to other functions
#'
#' @return        a (DelayedM|m)atrix of TPM estimates, or an object with one.
#'
#' @seealso http://bioinformatics.oxfordjournals.org/content/26/4/493.full
#' 
#' @examples
#' library(recount)
#' RPKMtoTPM(getRPKM(rse_gene_SRP009615))
#'
#' @import DelayedMatrixStats
#' @import Matrix
#' 
#' @export 
RPKMtoTPM <- function(object, ...) { 

  if (is(object, "DelayedMatrix")) {
    normalizers <- colSums2(object)
  } else if (is(object, "Matrix")) {
    normalizers <- colSums2(object)
  } else if (is(object, "matrix")) {
    normalizers <- colSums(object)
  } else { 
    stop("Don't know how to compute TPM for an object of class ", class(object))
  }
  sweep(rpkm, 2, normalizers, `/`) * 1e6

}
