#' Short term energy
#'
#' Computes the short term energy of a Wave.
#'
#' @param wave A Wave object
#' @param method Which method used to calculate the short term energy,
#' by default dietrich2004 to use Dietrich (2004) <doi:10.1016/j.patcog.2004.04.004>.
#' @param ... Other arguments to pass to STE function
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
    ste_dietrich2004(wave, ...)
  }
}
