#' Short term energy
#'
#' Computes the short term energy of a Wave.
#'
#' @param wave A Wave object
#' @param method Which method used to calculate the short term energy,
#' by default "dietrich2004" to use \insertCite{dietrich2004}{sonicscrewdriver}.
#' @param U Window length in samples, matching the pulse detection functions.
#' @param ... Other arguments to pass to ste method.
#' @references
#'   \insertAllCited{}
#' @export
#' @return A vector of short term energy values
#' @examples
#' \dontrun{
#' ste(sheep, method="dietrich2004")
#' }
#'
ste <-  function(
  wave,
  method="dietrich2004",
  U=120,
  ...
){
  .validateChoice(method, c("dietrich2004"), "method", "ste", prep="for")
  return(.ste_dietrich2004(wave, U=U, ...))
}

.ste_dietrich2004 <- function(wave, U=120) {
  # The energy at sample i is the sum of abs(wave@left) over a window of U+1
  # samples centred on i, so a cumulative sum gives every value in one pass.
  # For odd U the window sits half a sample to the left of centre.
  x <- abs(wave@left)
  n <- length(wave)
  before <- floor(U/2)
  after <- ceiling(U/2)

  e <- vector(mode="numeric", length=n)
  if (n - after < before + 1) {
    return(e)
  }

  i <- (before + 1):(n - after)
  cs <- c(0, cumsum(x))
  e[i] <- cs[i - before + U + 1] - cs[i - before]
  return(e)
}
