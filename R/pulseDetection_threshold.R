#' Pulse detection by thresholding the sample magnitudes
#'
#' Reports a pulse wherever the magnitude of the wave rises above a fraction of
#' its peak. Onsets and offsets follow the convention described under
#' pulseDetection(): an onset is the first sample above the threshold and an
#' offset the first sample after the pulse that is not, so offset - onset is the
#' length of the pulse in samples. Unlike the energy detectors this one
#' thresholds the samples themselves, so its boundaries are exact.
#'
#' @param wave A Wave object.
#' @param threshold The fraction of the peak magnitude a sample must exceed to be
#'   part of a pulse. With pd = TRUE the magnitudes are squared, so the threshold
#'   is a fraction of the peak power rather than of the peak amplitude and a
#'   threshold of 0.5 selects samples above 0.707 of the peak amplitude.
#' @param pd Whether to threshold the power rather than the amplitude.
#' @param U The minimum separation between the onsets of two reported pulses, in
#'   samples. A pulse whose onset falls within U samples of the last reported
#'   onset is discarded whole, with its offset.
#' @return A list of the onset and offset positions of the pulses found.
#' @noRd
pd_threshold <- function(wave, threshold=0.2, pd=FALSE, U=1) {
  mag <- wave@left
  if (pd==TRUE) {
    mag <- mag ^ 2
  } else {
    mag <- abs(mag)
  }

  #An NA sample makes max(mag) NA, so every comparison below is NA and which()
  #drops them all: the function reported no pulses at all for a wave that plainly
  #has them, without a word. There is no sensible threshold for a wave whose peak
  #is unknown, so say so instead.
  if (anyNA(mag)) {
    stop("Wave contains NA samples, so pd_threshold() cannot find a threshold.")
  }
  #max() of no samples warns and gives -Inf, which would make every sample of a
  #wave that has none a pulse.
  if (length(mag) == 0) {
    return(list(
      onsets = integer(0),
      offsets = integer(0)
    ))
  }
  threshold <- threshold * max(mag)

  #A sample exactly equal to the threshold is not part of a pulse, so every
  #sample is on one side of it or the other. Testing mag > threshold and
  #mag < threshold separately left such a sample on neither side, and a crossing
  #that passed through one was detected in neither direction: a pulse with a
  #shoulder at exactly half its peak gave an offset with no onset at all. The
  #first sample of the wave can open a pulse, which the zero formerly prepended
  #to the magnitudes was there to allow.
  runs <- .pulseRuns(mag > threshold)

  #A pulse is reported only if no pulse has been reported within the preceding U
  #samples, and it is kept or discarded whole. Thinning the onsets and the
  #offsets in two independent passes kept a different subset of the pulses in
  #each, because the spacing of the onsets is the pulse period while the spacing
  #of the offsets is the period plus the change in duration: three pulses of
  #unequal length returned the third pulse's onset beside the second's offset,
  #and a duration of -10 samples.
  accepted <- .pd_threshold_debounce(runs$onsets, U)
  return(list(
    onsets = runs$onsets[accepted],
    offsets = runs$offsets[accepted[accepted <= length(runs$offsets)]]
    )
  )
}

#' Discard pulses that follow too closely on a reported one
#'
#' A pulse is only reported if no pulse has already been reported within the
#' preceding U samples. Reported onsets are increasing, so only the most recent
#' one has to be considered.
#'
#' @param onsets A vector of pulse onset positions, in increasing order.
#' @param U Minimum separation between reported onsets, in samples.
#' @return The positions within onsets of the pulses that are reported.
#' @noRd
.pd_threshold_debounce <- function(onsets, U) {
  accepted <- vector(mode="integer", length=length(onsets))
  found <- 0
  last <- -Inf
  for (i in seq_along(onsets)) {
    if (onsets[i] - last > U) {
      found <- found + 1
      accepted[found] <- i
      last <- onsets[i]
    }
  }
  return(accepted[seq_len(found)])
}
