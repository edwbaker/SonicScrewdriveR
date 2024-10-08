#' Helper function to convert a Wave-like object to scikit-maad spectrogram
#' @param object A Wave or WaveMC object (or Tagged equivalent),
#'   or a spectrogram_maad object
#' @return A spectrogram_maad object
.spectrogram_maad_power <- function(object) {
  if (inherits(object, "spectrogram_maad")) {
    return(object)
  }
  if (inherits(object, c("Wave", "WaveMC"))) {
    return(maad_spectrogram(object))
  }
}

.spectrogram_maad_dB <- function(object) {
  spec <- .spectrogram_maad_power(object)
  spec@Sxx <- 10 * log10(spec@Sxx)
  spec@mode = "dB"
  return(spec)
}

#' Helper function to convert a Wave-like object to scikit-maad spectrum
#' @param object A Wave or WaveMC object (or Tagged equivalent),
#'   or a spectrum
#' @return A spectrum list
.spectrum_maad <- function(object) {
  if (inherits(object, "list")) {
    return(object)
  }
  if (inherits(object, c("Wave", "WaveMC"))) {
    return(maad_spectrum(object))
  }
}
