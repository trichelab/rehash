#' Encrypt metadata for a rehashed dataset with GPG, for secure transfers
#' 
#' For users who have a GPG keyring with entries for both themselves (sender)
#' and their collaborator (recipient), this function and its counterpart, 
#' \code{\link[rehash]{decryptMeta}}, allow relatively secure transfer of 
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
#' @param   meta  the metadata to decrypt (an object or a .gpg file)
#' 
#' @return  an encrypted metadata decoder (or, really, any object)
#'
#' @examples
#' data(exampleSE)
#' rehashed <- rehash(exampleSE)
#' object <- rehashed$object
#' encrypted <- encryptMeta(rehashed$meta, recipient="Tim Triche, Jr.")
#' decrypted <- decryptMeta(encrypted)
#' identical(decrypted, rehashed$meta)
#' rehashed <- NULL # no peeking!
#' dehash(object, decrypted) 
#' 
#' @seealso \code{\link[rehash]{decryptMeta}}
#' @seealso \code{\link[gpg]{gpg_decrypt}}
#' @seealso \code{\link[gpg]{gpg_encrypt}}
#' 
#' @import gpg
#' 
#' @export
encryptMeta <- function(meta, recipient) { 
  charToRaw(gpg_encrypt(serialize(meta, NULL), receiver=recipient))
}
