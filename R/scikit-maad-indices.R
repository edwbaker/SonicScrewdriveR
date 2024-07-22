#' Compute the temporal envelope median using scikit-maad
#'
#' For addition documentation see
#' \url{https://scikit-maad.github.io/generated/maad.features.temporal_median.html}.
#'
#' @param wave A Wave object.
#' @param mode Mode of the envelope calculation. Can be "fast" or "Hilbert".
#' @param Nt Size of each frame. The largest, the highest is the approximation.
#' @param maad An optional maad object. If not provided, one will be created using \code{getMaad()}.
#' @return Numeric median of the envelope.
#' @export
maad_temporal_median <- function(wave, mode="fast", Nt=512, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }
  ret <- maad$features$temporal_median(maad_wave(wave), mode=mode, Nt=as.integer(Nt))
  return(ret)
}

#' Compute the temporal entropy using scikit-maad
#'
#' For addition documentation see
#' \url{https://scikit-maad.github.io/generated/maad.features.temporal_entropy.html}.
#'
#' @param wave A Wave object.
#' @param compatibility One of "QUT" \insertCite{qut}{sonicscrewdriver}, "seewave" \insertCite{seewave2008}{sonicscrewdriver}.
#' @param mode Mode of the envelope calculation. Can be "fast" or "Hilbert".
#' @param Nt Size of each frame. The largest, the highest is the approximation.
#' @param maad An optional maad object. If not provided, one will be created using \code{getMaad()}.
#' @return Numeric entropy of the envelope.
#' @references
#'  \insertAllCited{}
#' @export
maad_temporal_entropy <- function(wave, compatibility="QUT", mode="fast", Nt=512, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }
  ret <- maad$features$temporal_entropy(maad_wave(wave), compatibility=compatibility, mode=mode, Nt=as.integer(Nt))
  return(ret)
}

#' Compute the acoustic activity index using scikit-maad
#'
#' For addition documentation see
#' \url{https://scikit-maad.github.io/generated/maad.features.temporal_activity.html}
#' \insertCite{towsey2013}{sonicscrewdriver}.
#'
#' @param wave A Wave object.
#' @param dB_threshold dB threshold of activity (default = 3).
#' @param mode Mode of the envelope calculation. Can be "fast" or "Hilbert".
#' @param Nt Size of each frame. The largest, the highest is the approximation.
#' @param maad An optional maad object. If not provided, one will be created using \code{getMaad()}.
#' @return List of calculated values, comprising:
#' \item{ACTfrac}{fraction)of points above the threshold for each frequency bin.}
#' \item{ACTcount}{total number of points above the threshold for each frequency bin.}
#' \item{ACTmean}{mean value (in dB) of the portion of the signal above the threshold.}
#' @references
#'  \insertAllCited{}
#' @export
maad_temporal_activity <- function(wave, dB_threshold=3, mode="fast", Nt=512, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }
  ret <- maad$features$temporal_activity(maad_wave(wave), dB_threshold=as.numeric(dB_threshold), mode=mode, Nt=as.integer(Nt))
  names(ret) <- c("ACTfrac", "ACTcount", "ACTmean")
  return(ret)
}

#' Compute the acoustic event index using scikit-maad
#'
#' An acoustic event corresponds to the period of the signal above a threshold.
#' An acoustic event could be short (at list one point if rejectDuration is
#' None) or very long (the duration of the  entire audio). Two acoustic events
#' are separated by a period with low audio signal (i.e. below the threshold).
#'
#' For addition documentation see
#' \url{https://scikit-maad.github.io/generated/maad.features.temporal_events.html}
#' \insertCite{towsey2013}{sonicscrewdriver} \insertCite{qut}{sonicscrewdriver}.
#'
#' @param wave A Wave object.
#' @param dB_threshold dB threshold of activity (default = 3).
#' @param rejectDuration Duration of the silence to reject an acoustic event (default = 0.1).
#' @param mode Mode of the envelope calculation. Can be "fast" or "Hilbert".
#' @param Nt Size of each frame. The larger, the highest is the approximation.
#' @param maad An optional maad object. If not provided, one will be created using \code{getMaad()}.
#' @return List of calculated values, comprising:
#' \item{EVTfrac}{fraction of points above the threshold for each frequency bin.}
#' \item{EVTcount}{total number of points above the threshold for each frequency bin.}
#' \item{EVTmean}{mean value (in dB) of the portion of the signal above the threshold.}
#' \item{EVN}{logical vector or matrix with 1 corresponding to event position.}
#' @references
#' \insertAllCited{}
#' @export
maad_temporal_events <- function(wave, dB_threshold=3, rejectDuration=0.1, mode="fast", Nt=512, maad=NULL) {
  if (is.null(maad)) {
    maad <- getMaad()
  }
  ret <- maad$features$temporal_events(maad_wave(wave), fs=wave@samp.rate, dB_threshold=as.numeric(dB_threshold), rejectDuration=as.numeric(rejectDuration), mode=mode, Nt=as.integer(Nt))
  names(ret) <- c("EVTfrac", "EVTcount", "EVTmean", "EVN")
  return(ret)
}

#' Compute the acoustic complexity index using scikit-maad
#'
#' ACI depends on the duration of the spectrogram as the derivation of the signal
#' is normalized by the sum of the signal. Thus, if the background noise is high
#' due to high acoustic activity the normalization by the sum of the signal
#' reduced ACI. So ACI is low when there is no acoustic activity or high
#' acoustic activity with continuous background noise. ACI is high only when
#' acoustic activity is medium, with sounds well above the background noise.
#'
#' For addition documentation see
#' \url{https://scikit-maad.github.io/generated/maad.features.temporal_acoustic_complexity_index.html}
#' \insertCite{pieretti2011}{sonicscrewdriver}.
#'
#' @param object A Wave object or a spectrogram_maad object generated by
#'   \code{\link{maad_spectrogram}}. If a Wave-like object is provided, the
#'   spectrogram will be calculated using the default parameters.
#' @return List comprising:
#' \item{ACI_xx}{Acoustic Complexity Index.}
#' \item{ACI_per_bin}{Acoustic Complexity Index.}
#' \item{ACI_sum}{Sum of ACI value per frequency bin (Common definition)}
#' @references
#'   \insertAllCited{}
#' @export
maad_acoustic_complexity_index <- function(object) {
  maad <- getMaad()
  if (inherits(object, c("Wave", "WaveMC"))) {
    object <- maad_spectrogram(object)
  }
  ret <- maad$features$acoustic_complexity_index(object@Sxx)
  names(ret) <- c("ACI_xx", "ACI_per_bin", "ACI_sum")
  return(ret)
}
