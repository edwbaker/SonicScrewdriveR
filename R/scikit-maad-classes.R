#' An S4 class to represent a spectrogram from scikit-maad.
#'
#' @slot Sxx The spectrogram matrix
#' @slot tn The time vector
#' @slot fn The frequency vector
#' @slot extents The extents of the spectrogram. A list of tmin, tmax, fmin, fmax.
setClass(
  "spectrogram_maad",
  slots=list(
    Sxx="matrix",
    tn="numeric",
    fn="numeric",
    extents="list",
    mode="character"
  ),
  prototype = list(
    Sxx = matrix(0, nrow=0, ncol=0),
    tn = numeric(0),
    fn = numeric(0),
    extents = list(),
    mode = "power"
  )
)

#' Helper function to convert a Wave-like object to scikit-maad spectrogram
#' @param object A Wave or WaveMC object, or a spectrogram_maad object
#' @return A spectrogram_maad object
spectrogram_maad <- function(object) {
  if (inherits(object, "spectrogam_maad")) {
    return(object)
  }
  if (inherits(object, c("Wave", "WaveMC"))) {
    return(maad_spectrogram(object))
  }
}
