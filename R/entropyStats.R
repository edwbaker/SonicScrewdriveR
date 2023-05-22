#' Various measurements of frequency values for a Wave object
#'
#' Calculates the peak, centre, bandwidth and quality factor. The quality factor (Q) is calculated at both
#' -3dB and -10dB as discussed by Bennett-Clark (1999)  <doi:10.1080/09524622.1999.9753408>.
#'
#' @param wave A Wave object
#' @importFrom seewave meanspec sh
#' @export
#' @return A list of spectral entropy types.
#' @examples
#' \dontrun{
#' entropyStats(sheep)
#' }
#'
entropyStats <- function(wave) {
  validateIsWave(wave)

  spec <- meanspec(wave)

  data = list(
    "spectral" = list(
      "shannon" = sh(spec, alpha="shannon"),
      "simpson" = sh(spec, alpha="simpson"),
      "renyi 2" = sh(spec, alpha=2),
      "renyi 3" = sh(spec, alpha=3)
    )
    #"temporal" = th(env(wave, plot=FALSE), breaks="Sturges"),
    #"total" = H(wave)
  )

  return(data)
}
