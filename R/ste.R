#' Short term energy
#'
#' Computes the short term energy of a Wave.
#' 
#' @param wave A Wave object
#' @param method Which method used to calculate the short term energy, by default dietrich2004 to use Dietrich (2004).
#' @param ... Other arguments to pass to STE function
#' @export
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