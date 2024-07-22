#' Compute a spectrogram of a sound wave using scikit-maad
#'
#' This is a wrapper function for the \code{maad.sound.spectrogram} function
#' from the scikit-maad package for Python. It computes the spectrogram of a
#' sound wave. Further usage details are provided at
#' \url{https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrogram}.
#'
#' @param wave A Wave object
#' @param ... Additional arguments to pass to \code{maad.sound.spectrogram}.
#' @param maad An optional \code{maad} object. If not provided, one will be created using \code{getMaad()}.
#' @return Generically a \code{spectrogram_maad} object.
#' @export
maad_spectrogram <- function(wave, ..., maad=NULL) {
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
    extents=ret$extents
  )

  return(spec)
}

maad_wave <- function(wave) {
 #ToDo: Process wave to be expected as maad.
    return(reticulate::np_array(wave@left))
}
