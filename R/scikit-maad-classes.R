#' Helper function to convert a Wave-like object to scikit-maad spectrogram
#'
#' A `spectrogram_maad` records in its `mode` slot which units its `Sxx` holds,
#' and the indices that want power have to be given power whatever the caller
#' built. Each mode is therefore converted rather than taken at face value: an
#' amplitude spectrogram is squared, a complex one has its magnitude squared,
#' and one already in dB is undone. Reading the slot is what makes the two
#' converters idempotent, so that a spectrogram passed through either of them
#' twice is not converted twice.
#'
#' @param object A Wave or WaveMC object (or Tagged equivalent),
#'   or a spectrogram_maad object
#' @return A spectrogram_maad object holding power, with its mode slot saying so.
.spectrogram_maad_power <- function(object) {
  if (inherits(object, "spectrogram_maad")) {
    if (identical(object@mode, "power")) {
      return(object)
    }
    object@Sxx <- switch(
      object@mode,
      amplitude = object@Sxx^2,
      complex = Mod(object@Sxx)^2,
      dB = 10^(object@Sxx/10),
      stop(paste0(
        "A spectrogram_maad in mode '", object@mode,
        "' cannot be converted to power."
      ))
    )
    object@mode <- "power"
    return(object)
  }
  if (inherits(object, c("Wave", "WaveMC"))) {
    return(maad_spectrogram(object))
  }
}

#' Helper function to convert a Wave-like object to a dB scikit-maad spectrogram
#'
#' One already in dB is returned as it is. Converting it again would not merely
#' give the wrong number: dB values are commonly zero or negative, so a second
#' `10*log10()` returns `-Inf` and `NaN`, which then travel into every index
#' built on the spectrogram.
#'
#' @param object A Wave or WaveMC object (or Tagged equivalent),
#'   or a spectrogram_maad object
#' @return A spectrogram_maad object holding dB, with its mode slot saying so.
.spectrogram_maad_dB <- function(object) {
  if (inherits(object, "spectrogram_maad") && identical(object@mode, "dB")) {
    return(object)
  }
  spec <- .spectrogram_maad_power(object)
  spec@Sxx <- 10 * log10(spec@Sxx)
  spec@mode <- "dB"
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
