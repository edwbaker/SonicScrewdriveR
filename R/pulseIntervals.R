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
  if (length(diffs) == 0) {
    return(list("onsets" = numeric(0), "offsets" = numeric(0)))
  }

  m <- mean(diffs)
  s <- stats::sd(diffs)
  #A single interval has no standard deviation, and cannot be an outlier of a set
  #of one.
  if (is.na(s)) {
    odds <- rep(FALSE, length(diffs))
  } else {
    odds <- diffs > m + nsd*s | diffs < m - nsd*s
  }

  #The gap measured by diffs[i] runs from the ith onset to the one after it. The
  #results were previously written at the index of the interval being examined
  #rather than of the interval being kept, which left the output padded with
  #zeroes and paired each gap with the onset before the one it started at.
  found <- which(odds)
  return(list(
    "onsets" = pulses$onsets[found],
    "offsets" = pulses$onsets[found + 1]
  ))
}
