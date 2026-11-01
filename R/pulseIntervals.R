#' Pulse intervals
#'
#' Used to locate areas of no pulses in the result of a pulseDetection().
#'
#' @details
#' A gap runs from the offset of one pulse to the onset of the next, following
#' the convention described under [pulseDetection()], so it holds exactly the
#' samples that are not part of a pulse and its length is
#' `onsets[i + 1] - offsets[i]`. n pulses have n - 1 gaps between them, and
#' neither no pulses nor one pulse has any, so nothing is reported for fewer
#' than two. A pulse the recording ends part way through has an onset and no
#' offset, and no gap follows it.
#'
#' A gap is reported if it is more than `nsd` standard deviations above the mean
#' gap. The test is one-sided, as an unusually short gap is not an area of no
#' pulses. Each gap is itself one of the values the mean and the standard
#' deviation are taken from, so it lifts the cut-off it has to clear, and the
#' most any one gap of k can reach is (k - 1)/sqrt(k): with the default `nsd` of
#' 2 nothing can be reported until there are 6 gaps, which is 7 pulses. A call
#' that cannot report anything whatever the gaps are says so.
#'
#' @param pulses The result of a pulseDetection(), holding onsets and offsets.
#' @param nsd The number of standard deviations above the mean gap that a gap
#'   must be to be reported as an area of no pulses.
#' @export
#' @return A list of the onset and offset of each area of no pulses: the first
#'   sample that is not part of a pulse, and the first sample of the next pulse.
#'
pulseIntervals <- function(
  pulses,
  nsd=2
){
  #The gap between two pulses runs from where one ends to where the next begins.
  #Measuring it from the onset before it instead put the whole of that pulse
  #inside the interval reported as holding no pulses, an error of one pulse
  #duration, and the offsets handed in were never read at all.
  if (is.null(pulses$offsets)) {
    stop("pulses must hold offsets as well as onsets, as a gap between pulses begins where a pulse ends.")
  }
  #n pulses have n-1 gaps, and the last pulse has no gap after it. A pulse the
  #recording ends part way through has no offset, so no gap can be measured from
  #it either.
  paired <- min(length(pulses$offsets), length(pulses$onsets) - 1)
  if (paired < 1) {
    return(list("onsets" = numeric(0), "offsets" = numeric(0)))
  }
  starts <- pulses$offsets[seq_len(paired)]
  ends <- pulses$onsets[seq_len(paired) + 1]
  gaps <- ends - starts

  m <- mean(gaps)
  s <- stats::sd(gaps)
  #The most standard deviations from the mean that one gap of k can reach is
  #(k-1)/sqrt(k), because the gap is one of the values the mean and the standard
  #deviation are taken from and so lifts the cut-off it has to clear. Below that
  #no gap can be reported however long it is, which the function used to do
  #without a word: with the default nsd of 2, six or fewer gaps always gave an
  #empty result, however obvious the gap.
  reach <- (paired - 1)/sqrt(paired)
  if (nsd >= reach) {
    warning(paste0(
      "No gap can be reported from ", paired,
      if (paired == 1) " gap " else " gaps ",
      "with nsd=", nsd, ": the most standard deviations from the mean that one ",
      "gap of ", paired, " can reach is ", signif(reach, 4),
      ", as the gap is one of the values the mean and the standard deviation are ",
      "taken from. A smaller nsd, or more pulses, is needed."
    ))
  }
  #A single gap has no standard deviation, and cannot be an outlier of a set of
  #one.
  if (is.na(s)) {
    odds <- rep(FALSE, paired)
  } else {
    #One-sided: a gap that is unusually short is the opposite of an area of no
    #pulses. The other arm of the test reported a gap of 10 samples between
    #pulses 2000 samples apart, an interval that lay inside a pulse.
    odds <- gaps > m + nsd*s
  }

  found <- which(odds)
  return(list(
    "onsets" = starts[found],
    "offsets" = ends[found]
  ))
}
