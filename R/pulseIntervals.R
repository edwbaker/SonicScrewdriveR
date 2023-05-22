#' Pulse intervals
#'
#' Used to locate area of no pulses from the results of pulseDetection().
#'
#' @param pulses The result of a pulseDetection.
#' @param nsd The number of standard deviations each sid of the mean pulse interval to discard
#' @export
#' @return A list of onset and offset times for pulses
#'
pulseIntervals <- function(
  pulses,
  nsd=2
){
  diffs <- diff(pulses$onsets)
  m <- mean(diffs)
  s <- stats::sd(diffs)
  nsd <- 2

  odds <- diffs > m + nsd*s | diffs < m-nsd*s

  n_intervals <- length(which(odds[odds==TRUE]))
  onsets <- vector(mode="numeric", length=n_intervals)
  offsets <- onsets

  for (i in 2:length(odds)) {
    if (odds[i] == TRUE) {
      onsets[i] <- pulses$onsets[i-1]
      offsets[i] <- pulses$onsets[i-1]+diffs[i]
    }
  }
  return(list(
    "onsets" = onsets,
    "offsets" = offsets
  ))
}
