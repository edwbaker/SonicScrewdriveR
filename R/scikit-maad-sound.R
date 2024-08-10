#' Compute a spectrogram of a sound wave using scikit-maad
#'
#' This is a wrapper function for the \code{maad.sound.spectrogram} function
#' from the scikit-maad package for Python. It computes the spectrogram of a
#' sound wave. Further usage details are provided at
#' \url{https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrogram}.
#'
#' @param wave A Wave object
#' @param mode The type of spectrogram to compute. Options are "power", "amplitude" "complex".
#'   Default is "power".
#' @param ... Additional arguments to pass to \code{maad.sound.spectrogram}.
#' @param maad An optional \code{maad} object. If not provided, one will be created using \code{getMaad()}.
#' @return Generically a \code{spectrogram_maad} object.
#' @export
maad_spectrogram <- function(wave, mode="power", ..., maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }

  ret <- maad$sound$spectrogram(x=maad_wave(wave), fs=wave@samp.rate, ...)
  names(ret) <- c("Sxx", "tn", "fn","extents")
  names(ret$extents) <- c("tmin", "tmax", "fmin", "fmax")

  spec<- new(
    "spectrogram_maad",
    Sxx = ret$Sxx,
    tn = as.numeric(ret$tn),
    fn = as.numeric(ret$fn),
    extents=ret$extents,
    mode=mode
  )

  return(spec)
}

maad_wave <- function(wave) {
 #ToDo: Process wave to be expected as maad.
  if (inherits(wave, c("WaveMC", "TaggedWaveMC"))) {
    return(reticulate::np_array(wave@`.Data`[,1]))
  } else if (inherits(wave, c("Wave", "TaggedWave"))) {
    return(reticulate::np_array(wave@left))
  }
}

#' Compute a spectrum of a sound wave using scikit-maad
#'
#' This is a wrapper function for the \code{maad.sound.spectrum} function.
#' It computes the spectrum of a sound wave. Further usage details are provided at
#' \url{https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrum}.
#'
#' @param wave A Wave object
#' @param ... Additional arguments to pass to \code{maad.sound.spectrum}.
#' @param maad An optional \code{maad} object. If not provided, one will be created using \code{getMaad()}.
#' @return A list comprising:
#' \item{pxx}{Power spectral density estimate.}
#' \item{f_idx}{Index of sample frequencies.}
#' @export
maad_spectrum <- function(wave, ..., maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }

  ret <- maad$sound$spectrum(s=maad_wave(wave), fs=wave@samp.rate, ...)
  names(ret) <- c("pxx", "f_idx")
  return(ret)
}
