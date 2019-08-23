#' anonymize (to some degree) a SummarizedExperiment (or an object of a class 
#' derived from SummarizedExperiment) for quasi-public transfer/dissemination
#' 
#' This is NOT cryptographically secure nor equivalent to a proper 2-key de-ID!
#' It is likely to dissuade casual attack, but cannot stop a motivated attacker.
#' 
#' Specialized functions for rehash'ing specialized SE-like objects and for 
#' providing key-exchangeable versions of this functionality are forthcoming.
#'
#' Assay renaming currently works by matching the assay name to the actual 
#' hdf5 path name used in HDF5 backing files (assays.h5), as produced by 
#' HDF5Array::saveHDF5SummarizedExperiment(...).  This should ease interop
#' with e.g. Python consumers of the data (they'll still need reverse mappings
#' for the column and row names, but that's not too terribly difficult either). 
#' 
#' At some point, it may make more sense to save metadata for rehash/dehash 
#' purposes to a relatively language-agnostic data format like Feather, or 
#' else break up all the pieces into CSVs and write a Python package to handle
#' the reversing of hash-mappings. Either should be fine for interop. 
#' 
#' @param   x           a [Ranged]SummarizedExperiment to anonymize 
#' @param   salt        a salting phrase to slow brute-force attacks ("0x")
#' @param   strip       strip rehashed objects of any deID'ing metadata? (TRUE)
#' @param   algo        algorithm to use for the one-way hash (default is "md5")
#' @param   deorder     scramble rows and columns? (FALSE; disrupts data digest)
#' 
#' @return              an object of the same class as x, with hashed dimnames
#' 
#' @aliases rehashSummarizedExperiment
#' 
#' @import  SummarizedExperiment
#' @import  digest
#' 
#' @examples 
#' 
#' ncols <- 6
#' nrows <- 200
#' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' rownames(counts) <- apply(expand.grid(letters, letters), 1, 
#'                           paste0, collapse="")[seq_len(nrow(rse))]
#' rowRanges <- GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#'                      IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#'                      strand=sample(c("+", "-"), 200, TRUE),
#'                      feature_id=sprintf("ID%03d", 1:200))
#' names(rowRanges) <- rownames(counts) 
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#'
#' # a toy RangedSummarizedExperiment (?SummarizedExperiment) 
#' rse <- SummarizedExperiment(assays=SimpleList(counts=counts),
#'                             rowRanges=rowRanges, colData=colData)
#' assays(rse)$cpm <- sweep(assays(rse)$counts * 1e6, 2, normalizers, `/`)
#' covs <- colData(rse) # alternative to pulling these from res$covs
#'
#' # rehash the toy RangedSummarizedExperiment:
#' res <- rehash(rse, salt="testing", strip=TRUE, algo="md5", deorder=FALSE)
#' deIDed <- res$object
#' 
#' # test it out with HDF5-backed storage:
#' library(HDF5Array)
#' deIDedPath <- file.path(tempdir() , "deIDed") 
#' deIDed <- saveHDF5SummarizedExperiment(deIDed, deIDedPath, replace=TRUE)
#'
#' # recover the rehashed object using the saved metadata:
#' meta <- res$meta
#' covs <- res$covs
#' reIDed <- dehash(deIDed, meta=meta, covs=covs, check=TRUE)
#'
#' if (!is.null(colnames(rse))) {
#'   stopifnot(identical(colnames(reIDed), colnames(rse)))
#' } 
#'
#' if (!is.null(rownames(rse))) {
#'   stopifnot(identical(rownames(reIDed), rownames(rse)))
#' } 
#'
#' # seeing is believing
#' show(reIDed)
#' 
#' @export
rehashSE <- function(x, salt="0x", strip=TRUE, algo="md5", deorder=FALSE) { 

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


  # assays are neither seasoned nor reordered but rather refer to HDF5 paths
  oldasys <- seq_along(assays(x))
  asys <- sprintf("assay%03d", oldasys)
  assaymap <- data.frame(original=oldasys, new=asys, 
                         ordering=seq_along(assays(x)))
  rownames(assaymap) <- asys

  # wash it
  newSE <- x

  rownames(newSE) <- feats
  if (is(rowRanges(newSE), "GRangesList")) {
    newRR <- unlist(reduce(rowRanges(newSE)))
    rowRanges(newSE) <- newRR[match(rownames(newSE), names(newRR))] 
  }
  rowData(newSE) <- DataFrame(feature=feats, row.names=feats)

  # store for stripped 
  covs <- colData(newSE)
  colnames(newSE) <- samps
  colData(newSE) <- DataFrame(sample=samps, row.names=samps)

  names(assays(newSE)) <- asys 
  assayRowHashes <- getAssayRowHashes(newSE, algo=algo)
  assayColHashes <- getAssayColHashes(newSE, algo=algo)
 
  # ok, let's assemble the magic decoder ring:  
  metadata(newSE) <- list(featuremap=featuremap,
                          samplemap=samplemap, 
                          assaymap=assaymap,
                          assayRowHashes=assayRowHashes,
                          assayColHashes=assayColHashes,
                          state="rehashed",
                          salted="samplemap",
                          deorder=deorder,
                          salt=salt,
                          algo=algo)

  if (deorder) newSE <- .deorder(newSE)
  if (strip) return(.stripSE(newSE, covs))
  else return(newSE)

}

# helper fn
.stripSE <- function(x, covs=NULL) { 
  meta <- metadata(x)
  metadata(x) <- list()
  res <- list(object=x, meta=meta)
  if (!is.null(covs)) res$covs <- covs
  return(res)
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
