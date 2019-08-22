#' strip dimnames (forcibly) for verifiable row/column/assay hash checking
#' 
#' @param x   a matrix 
#' 
#' @return    a matrix with no dimnames
#' 
#' @export
stripDimnames <- function(x) {
  dimnames(x) <- list(NULL, NULL)
  return(x)
}
