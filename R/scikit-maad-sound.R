#' Compute a spectrogram of a sound wave using scikit-maad
#'
#' This is a wrapper function for the \code{maad.sound.spectrogram} function
#' from the scikit-maad package for Python. It computes the spectrogram of a
#' sound wave. Further usage details are provided at
#' \url{https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrogram}.
#'
#' @param wave A Wave object
#' @param mode The type of spectrogram to compute. Options are "power", "amplitude" "complex".
#'   Default is "power". A "complex" spectrogram holds complex numbers, and the
#'   indices built on it take its magnitude.
#' @param ... Additional arguments to pass to \code{maad.sound.spectrogram}.
#' @param maad An optional \code{maad} object. If not provided, one will be created using \code{getMaad()}.
#' @return Generically a \code{spectrogram_maad} object.
#' @export
maad_spectrogram <- function(wave, mode="power", ..., maad=NULL) {
  maad <- .maad(maad)

  ret <- maad$sound$spectrogram(
    x=maad_wave(wave),
    fs=wave@samp.rate,
    mode=.maadSpectrogramMode(mode),
    ...
  )
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

#' The name scikit-maad knows a spectrogram mode by
#'
#' scikit-maad calls a power spectrogram "psd", and this package calls it
#' "power" throughout: in the `mode` slot of `spectrogram_maad`, in the
#' `spectrogram_maad_power` slot of a `TaggedWave` and in the name of
#' `.spectrogram_maad_power()`. The name is therefore translated on the way out
#' rather than changed on the way in.
#'
#' A mode scikit-maad does not know is refused here. Its own `spectrogram()`
#' tests the mode with three `if` statements and no `else`, so an unknown one
#' leaves the output it is about to return unassigned and raises
#' `UnboundLocalError: cannot access local variable 'Sxx_out'`, which names
#' neither the argument at fault nor the values it accepts.
#'
#' @param mode A mode as this package names it.
#' @return The mode as scikit-maad names it.
#' @noRd
.maadSpectrogramMode <- function(mode) {
  modes <- c(power="psd", amplitude="amplitude", complex="complex")
  if (!is.character(mode) || length(mode) != 1 || !mode %in% names(modes)) {
    stop(paste0(
      "mode must be one of ", paste(names(modes), collapse=", "), "."
    ))
  }
  return(modes[[mode]])
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
  maad <- .maad(maad)

  ret <- maad$sound$spectrum(s=maad_wave(wave), fs=wave@samp.rate, ...)
  names(ret) <- c("pxx", "f_idx")
  return(ret)
}
