#' Statistics of the pulses a detector reported
#'
#' Gives the length of each pulse and the distance from each pulse to the next,
#' in samples.
#'
#' Every detector in the package reports onsets[i] and offsets[i] as the two ends
#' of the same pulse, as described under pulseDetection(), so a duration is
#' offsets[i] - onsets[i] and a distance is onsets[i+1] - offsets[i], the samples
#' between one pulse and the next. A recording that ends part way through a pulse
#' leaves one more onset than offset, and that pulse is left out.
#'
#' @param pulseInfo The result of a pulseDetection().
#' @return A list of the durations of the pulses and the distances between them.
#' @noRd
pulseStats <- function(pulseInfo) {
  #A recording can end part way through a pulse, leaving one more onset than
  #offset, and 2:l counted backwards when there were fewer than two pulses.
  paired <- min(length(pulseInfo$onsets), length(pulseInfo$offsets))
  onsets <- pulseInfo$onsets[seq_len(paired)]
  offsets <- pulseInfo$offsets[seq_len(paired)]

  #An offset at or before the onset beside it means the two lists describe
  #different pulses, and the duration below would be negative or zero. A
  #duration is a length of time and cannot be either, and this used to return one
  #rather than say so: an extra offset at the front shifted every pair by one,
  #which is what pd_dietrich2004() returned for a recording beginning part way
  #through a pulse, giving durations of -2505 and -1505 samples.
  if (any(offsets <= onsets)) {
    stop("pulseInfo holds an offset at or before the onset beside it, so the onsets and offsets do not describe the same pulses.")
  }
  if (paired > 1 && any(onsets[-1] < offsets[-paired])) {
    stop("pulseInfo holds a pulse beginning before the pulse before it ends, so the onsets and offsets do not describe the same pulses.")
  }

  gaps <- seq_len(max(0, paired - 1))
  pulseDistances <- onsets[gaps + 1] - offsets[gaps]
  return(
    list(
      "durations" = offsets - onsets,
      "distances" = pulseDistances
    )
  )
}
