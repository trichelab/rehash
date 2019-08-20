#' deanonymize (to some degree) a SummarizedExperiment post public dissemination
#' 
#' Verifiable de-anonymization requires metadata: a sample map, a feature map,
#' a salting phrase, and knowledge of whether the salt phrase was applied to 
#' features, samples, both, or neither. If the metadata is left in the metadata
#' slot of the dehydrated SummarizedExperiment, it will be used, else it must 
#' be supplied by the user. Optionally, the data itself may be verified.
#'
#' @param   x       a [Ranged]SummarizedExperiment to deanonymize 
#' @param   meta    the meta-metadata required to reverse the hashes
#' @param   covs    a data.frame of covariates (rows are samples, columns covs)
#' 
#' @return        an object of the same class as x, but with cleartext dimnames
#' 
#' @aliases dehashSummarizedExperiment rehydrateSummarizedExperiment rehydrateSE
#' 
#' @export
dehashSE <- function(x, meta=NULL, covs=NULL) {
  
  if (is.null(meta)) meta <- metadata(x)
  if (length(meta) == 0) stop("Rehydration of an SE needs a metadata decoder.") 
  if (!all(c("samplemap","featuremap","assaymap") %in% names(meta))) {
    stop("Dehashing an SE requires `featuremap`, `samplemap`, and `assaymap`.")
  }

  # check reverse map: 
  if (!is.null(covs)) x <- .addCovs(x, covs)
  metadata(x)$featuremap <- .flipMap(meta$featuremap)
  metadata(x)$samplemap <- .flipMap(meta$samplemap)
  metadata(x)$assaymap <- .flipMap(meta$assaymap)
  metadata(x)$state <- "rehydrated"
  if (meta$deorder) {
    return(.reorder(x))
  } else { 
    checkAsys(x, meta)
    names(assays(x)) <- .getAsyNames(x, meta=meta)
    rownames(x) <- .getRowNames(x, meta=meta)
    colnames(x) <- .getColNames(x, meta=meta)
    return(x)
  }

}

# helper 
.flipMap <- function(x) { 
  mandatory <- c("original","new","ordering")
  stopifnot(all(mandatory %in% colnames(x)))
  stopifnot(identical(rownames(x), x[["new"]]))
  rownames(x) <- x$original
  x$original <- x$new
  x$new <- rownames(x)
  return(x)
}

# helper 
.reorder <- function(x, meta=NULL) {
  
  if (is.null(meta)) meta <- metadata(x)

  # check row indexing 
  if (!all(rownames(x) %in% rownames(meta$featuremap))) {
    meta$featuremap <- .flipMap(meta$featuremap)
  }
  stopifnot(all(rownames(x) %in% rownames(meta$featuremap)))

  # check column indexing 
  if (!all(colnames(x) %in% rownames(meta$samplemap))) {
    meta$samplemap <- .flipMap(meta$samplemap)
  }
  stopifnot(all(colnames(x) %in% rownames(meta$samplemap)))

  # check assay indexing 
  if (!all(names(assays(x)) %in% rownames(meta$assaymap))) {
    meta$assaymap <- .flipMap(meta$assaymap)
  }
  stopifnot(all(names(assays(x)) %in% rownames(meta$assaymap)))

  # restore original order
  rowOrder <- .getRowOrder(x, meta=meta)
  colOrder <- .getColOrder(x, meta=meta)
  rownames(x) <- .getRowNames(x, meta=meta)
  colnames(x) <- .getColNames(x, meta=meta)
  names(assays(x)) <- .getAsyNames(x, meta=meta)
  x[order(rowOrder), order(colOrder)]

}

# helper 
.FM <- function(x, meta=NULL) { 
  if (is.null(meta)) meta <- metadata(x)
  return(meta$featuremap) 
} 

# helper 
.getRowOrder <- function(x, meta=NULL) .FM(x, meta)[rownames(x), "ordering"] 

# helper 
.getRowNames <- function(x, meta=NULL) .FM(x, meta)[rownames(x), "original"]

# helper 
.SM <- function(x, meta=NULL) { 
  if (is.null(meta)) meta <- metadata(x)
  return(meta$samplemap) 
} 

# helper 
.getColOrder <- function(x, meta=NULL) .SM(x, meta)[colnames(x), "ordering"] 

# helper 
.getColNames <- function(x, meta=NULL) .SM(x, meta)[colnames(x), "original"]

# helper 
.AM <- function(x, meta=NULL) { 
  if (is.null(meta)) meta <- metadata(x)
  return(meta$assaymap) 
} 

# helper 
.getAsyOrder <- function(x, meta=NULL) {
  .AM(x, meta)[names(assays(x)), "ordering"] 
}

# helper 
.getAsyNames <- function(x, meta=NULL) {
  .AM(x, meta)[names(assays(x)), "original"]
}

# helper
.checkAsys <- function(x, meta=NULL) { 
  AM <- .AM(x, meta)
  md5s <- sapply(assays(x), digest)
  stopifnot(all(md5s == AM$md5))
}

# helper 
.addCovs <- function(x, covs) { 
  if (!"original" %in% colnames(covs)) {
    covs$original <- rownames(covs)
    rownames(covs) <- sapply(covs$original, 
                             indigestion, 
                             salt=with(meta, 
                                       ifelse(salted=="samplemap", salt, "")),
                             algo=meta$algo)
    if (!all(rownames(covs) %in% colnames(x))) stop("Sample name mismatch!")
    colData(x) <- DataFrame(covs[colnames(x),]) 
  }
  return(x) 
}

# alias
rehydrateSummarizedExperiment <- dehashSE 

# alias
rehashSummarizedExperiment <- dehashSE 

# alias
rehydrateSE <- dehashSE 
