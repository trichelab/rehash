#' Decrypt metadata for a rehashed dataset with GPG, for secure transfers
#' 
#' For users who have a GPG keyring with entries for both themselves (sender)
#' and their collaborator (recipient), this function and its counterpart, 
#' \code{\link[rehash]{encryptMeta}}, allow relatively secure transfer of 
#' metadata associated with a rehashed dataset, to keep it within a set of 
#' trustworthy individuals until such time as it is appropriate to make public.
#' 
#' There are some important caveats: if recipients are not trustworthy (i.e. 
#' if one cannot trust collaborators), this precaution does not offer much in 
#' the way of assurance. However, if recipients are trustworthy, this can make
#' brute-force reidentification attacks too costly to be worthwhile, especially
#' if the data is meant for eventual public release at a later date. 
#' 
#' Note: this function assumes that sender and recipient have GPG keys 
#' available to one another. GPG (Gnu Privacy Guard) is an implementation of
#' PGP (Pretty Good Privacy). PGP is Phil Zimmerman's public-key cryptography 
#' scheme (see \url{https://en.wikipedia.org/wiki/Pretty_Good_Privacy}), used 
#' via the \url{https://cran.r-project.org/web/packages/gpg/}{gpg R package}.
#' 
#' FIXME: make it possible to use a .gpg file for the transport format. 
#' 
#' @param   encrypted   the metadata to decrypt (an object or a .rds file)
#' 
#' @return              metadata decoder for \code{\link[rehash]{dehash}}
#' 
#' @examples
#' data(exampleSE)
#' rehashed <- rehash(exampleSE)
#' object <- rehashed$object
#' encrypted <- encryptMeta(rehashed$meta, receiver="Tim Triche, Jr.")
#' decrypted <- decryptMeta(encrypted)
#' identical(decrypted, rehashed$meta)
#' rehashed <- NULL # no peeking!
#' dehash(object, decrypted) 
#' 
#' @seealso \code{\link[rehash]{encryptMeta}}
#' @seealso \code{\link[gpg]{gpg_encrypt}}
#' @seealso \code{\link[gpg]{gpg_decrypt}}
#' 
#' @import gpg
#' 
#' @export
decryptMeta <- function(meta) { 
  if (file.exists(meta)) {
    unserialize(gpg_decrypt(readRDS(encrypted), as_text=FALSE))
  } else { 
    unserialize(gpg_decrypt(encrypted, as_text=FALSE))
  }
}
