#' Short term energy
#'
#' Computes the short term energy of a Wave.
#'
#' @param wave A Wave object
#' @param method Which method used to calculate the short term energy,
#' by default "dietrich2004" to use \insertCite{dietrich2004}{sonicscrewdriver}.
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
  ...
){
  if (method == "dietrich2004") {
    .ste_dietrich2004(wave, ...)
  }
}

.ste_dietrich2004 <- function(wave, U) {
  e <- vector(mode="numeric", length=length(wave))
  for (i in (U/2+1):(length(wave)-U/2)) {
    values <- (i-U/2):(i+U/2)
    values <- values[values > 0]
    e[i] <- sum(abs(wave@left[values]))
  }
  return(e)
}
