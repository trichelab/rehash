#' turn an HDF5-backed SummarizedExperiment back into a usable GenomicRatioSet
#'
#' This function depends on the following to figure out how to annotate: 
#' 
#' minfi:::.default.27k.annotation
#' minfi:::.default.450k.annotation
#' minfi:::.default.epic.annotation
#'
#' If there's a disconnect between these and the truth, it will cause problems.
#'
#' @param  grSet    a SummarizedExperiment, often HDF5-backed, from a grSet
#' @param  HM       optional indicator of what platform grSet is made from 
#' 
#' @import minfi
#' 
#' @export 
revertGrSet <- function(grSet, HM=c("EPIC","HM450","HM27")) { 
  HM <- match.arg(HM)
  grSet <- as(grSet, "GenomicRatioSet")
  if (!all(c("array","annotation") %in% names(annotation(grSet)))) {
    annotation(grSet) <- 
      switch(HM, 
             EPIC=c(array="IlluminaHumanMethylationEPIC",
                    annotation=minfi:::.default.epic.annotation),
             HM450=c(array="IlluminaHumanMethylation450k",
                     annotation=minfi:::.default.450k.annotation),
             HM27=c(array="IlluminaHumanMethylation27k",
                    annotation=minfi:::.default.27k.annotation))
  }
  return(grSet)
}
