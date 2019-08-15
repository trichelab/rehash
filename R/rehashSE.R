#' anonymize (to some degree) a SummarizedExperiment (or an object of a class 
#' derived from SummarizedExperiment) for quasi-public transfer/dissemination
#' 
#' This is NOT cryptographically secure nor equivalent to a proper 2-key de-ID!
#' Also, a separate function for rehash'ing SingleCellExperiments is imminent.
#'
#' @param   x      a [Ranged]SummarizedExperiment to anonymize 
#' @param   salt    a salting phrase to slow brute-force metadata attacks ("0x")
#' @param   algo    algorithm to use for the one-way hash (default is "md5") 
#' @param   deorder scramble the rows and columns? (TRUE) 
#' 
#' @return          an object of the same class as x, but with hashed dimnames
#' 
#' @aliases dehydrateSummarizedExperiment rehashSummarizedExperiment dehydrateSE
#' 
#' @import  SummarizedExperiment
#' @import  digest
#' 
#' @examples 
#' 
#' nrows <- 200
#' ncols <- 6
#' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#'                      IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#'                      strand=sample(c("+", "-"), 200, TRUE),
#'                      feature_id=sprintf("ID%03d", 1:200))
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#'
#' rse <- SummarizedExperiment(assays=SimpleList(counts=counts),
#'                             rowRanges=rowRanges, colData=colData)
#' covs <- colData(rse)
#'
#' deIDed <- rehash(rse, salt="testing", algo="md5")
#' meta <- metadata(deIDed)
#' metadata(deIDed) <- list() 
#' 
#' library(HDF5Array)
#' deIDedPath <- file.path(tempdir() , "deIDed") 
#' deIDed <- saveHDF5SummarizedExperiment(deIDed, deIDedPath)
#'
#' reIDed <- dehash(deIDed, meta=meta, covs=covs)
#' reIDedPath <- file.path(tempdir() , "reIDed") 
#' reIDed <- saveHDF5SummarizedExperiment(deIDed, deIDedPath)
#' stopifnot(identical(colnames(reIDed), colnames(rse)))
#' stopifnot(identical(rownames(reIDed), rownames(rse)))
#' 
#' setMethod("counts", "SummarizedExperiment", 
#'   function(object) assay(object, which(names(assays(object)) == "counts")))
#' 
#' library(DelayedMatrixStats)
#' stopifnot(identical(colSums2(counts(reIDed)), colSums2(counts(rse))))
#' stopifnot(identical(rowSums2(counts(reIDed)), rowSums2(counts(rse))))
#' show(reIDed)
#' 
#' @export
rehashSE <- function(x, salt="0x", algo="md5", deorder=T){

  # features are unseasoned
  oldfeats <- seq_len(nrow(x))
  if (!is.null(rownames(x))) oldfeats <- rownames(x)
  feats <- rehash(oldfeats, algo=algo)
  if (any(duplicated(feats))) stop(paste0(algo, " collision in feature names!"))
  featuremap <- data.frame(original=oldfeats, new=feats, 
                           ordering=seq_along(feats))
  rownames(featuremap) <- feats

  # samples are seasoned
  oldsamps <- seq_len(ncol(x))
  if (!is.null(colnames(x))) oldsamps <- colnames(x)
  samps <- rehash(oldsamps, salt=salt, algo=algo)
  if (any(duplicated(samps))) stop(paste0(algo, " collision in sample names!"))
  samplemap <- data.frame(original=oldsamps, new=samps,
                          ordering=seq_along(samps))
  rownames(samplemap) <- samps

  # assays are neither seasoned nor reordered 
  oldasys <- seq_along(assays(x))
  if (!is.null(names(assays(x)))) oldasys <- names(assays(x))
  asys <- rehash(oldasys, algo=algo)
  if (any(duplicated(asys))) stop(paste0(algo, " collision in assay names!"))
  assaymap <- data.frame(original=oldasys, new=asys, 
                         ordering=seq_along(asys))
  rownames(assaymap) <- asys

  # wash it
  newSE <- x

  rownames(newSE) <- feats
  if (is(rowRanges(newSE), "GRangesList")) {
    newRR <- unlist(reduce(rowRanges(newSE)))
    rowRanges(newSE) <- newRR[match(rownames(newSE), names(newRR))] 
  }
  rowData(newSE) <- DataFrame(feature=feats, row.names=feats)

  colnames(newSE) <- samps
  colData(newSE) <- DataFrame(sample=samps, row.names=samps)

  names(assays(newSE)) <- asys 

  metadata(newSE) <- list(featuremap=featuremap, 
                          samplemap=samplemap, 
                          assaymap=assaymap,
                          state="dehydrated",
                          salted="samplemap",
                          salt=salt,
                          algo=algo)

  if (deorder) return(.deorder(newSE))
  else return(newSE) 

}

# helper fn
.deorder <- function(x) { 
  x <- x[sample(rownames(x), nrow(x)), sample(colnames(x), ncol(x))]
  metadata(x)$featuremap <- metadata(x)$featuremap[rownames(x), ] 
  metadata(x)$samplemap <- metadata(x)$samplemap[colnames(x), ] 
  return(x) 
}

# alias 
rehashSummarizedExperiment <- rehashSE

# alias 
dehydrateSummarizedExperiment <- rehashSE

# alias 
dehydrateSE <- rehashSE 
