#' Dolbear's law
#'
#' Calculates either chirps per minute based on temperature or vice versa
#' using Dolbear's law (or equivalent laws for other species)
#'
#' @param n Chirps per minute
#' @param t Temperature in Celsius
#' @param species Species to use (by default Oecanthus fultoni)
#' @return Missing value of n or t
#' @export
#' @examples
#' dolbear(n=6)
#' dolbear(t=25)
#'
dolbear <- function(n=NULL, t=NULL, species="Oecanthus fultoni") {
  if (is.null(n) & is.null(t)) {
    stop("Dolbear's law calculation requires either n or t to be specified.")
  }
  if (species=="Oecanthus fultoni") {
    if (is.null(t)) {
      return((n+30)/7)
    }
    if (is.null(n)) {
      return(7*t-30)
    }
  }
}
