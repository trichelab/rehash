#' dehash (if necessary) the file at an URL. Note: this function is NOT 'safe'!
#' 
#' Seriously -- if you aren't sure that you trust the owner of an URL and the 
#' payload, you MUST NOT apply this function to it. We'll try to snoop the file
#' header, but that is NOT strong protection. Download and inspect if unsure.
#' 
#' @import recount
#'
#' @export
dehashURL <- function(x, ...) {

  stop("Not implemented yet.") 

}
