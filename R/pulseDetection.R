# Pulse boundaries in this package
# --------------------------------
# Every pulse detector here reports a pulse as a pair of positions in the channel
# it analysed, counted from one, and all of them use the same convention:
#
#   onset   the first sample of the pulse
#   offset  the first sample after the pulse
#
# A pulse therefore occupies the half-open range from its onset up to but not
# including its offset, and offset - onset is its length in samples.
#
# A sample exactly equal to the detection threshold is not part of a pulse. The
# test is a strict "above threshold", and anything not above it is outside the
# pulse, so every sample is on exactly one side of the threshold and no crossing
# can fall between two tests and be lost.
#
# pd_dietrich2004() is the exception, and the only one. Dietrich (2004) writes
# its comparisons as "at least", and that function follows the paper, so a
# sample exactly equal to a threshold is inside a pulse there. It only shows on
# a wave that holds such a sample at all: a silent or constant amplitude wave,
# whose every energy is exactly equal to every threshold, is one pulse to
# pd_dietrich2004() and no pulse to the other two.
#
# onsets[i] and offsets[i] are always the two ends of the same pulse. The two
# vectors hold the same number of positions, except that a pulse still in
# progress at the last analysed sample has an onset and no offset, so onsets can
# be one longer than offsets and the unmatched onset is always the last one.
# Nothing else is ever reported unpaired, so pulseStats() can pair by position
# and every duration it reports is positive.
#
# A pulse already in progress at the first analysed sample is reported with its
# onset at that sample. pd_threshold() has always done this, by padding the
# magnitudes with zeroes before looking for crossings, and the energy detectors
# now agree with it.
#
# pd_simple() and pd_dietrich2004() threshold a windowed energy, and ste() only
# computes a window for the samples that hold a whole one, so their first and
# last analysed samples lie inside the wave rather than at its ends: they are
# blind to the half window at each end, and .steDefined() gives the samples they
# can see. Their boundaries are also displaced outward by up to half a window,
# which is described under pd_simple().

#' Pulse detection
#'
#' Detects pulses in a Wave. Stereo and multi-channel waves are analysed one
#' channel at a time, giving one result per channel.
#'
#' @details
#' Each method reports a pulse as an onset and an offset. An onset is the first
#' sample of the pulse and an offset the first sample after it, so the pulse
#' occupies the half-open range from onset to offset and `offset - onset` is its
#' length in samples. A sample exactly equal to the detection threshold is not
#' part of a pulse.
#'
#' `onsets[i]` and `offsets[i]` are always the two ends of the same pulse. A
#' pulse still in progress at the last analysed sample has an onset and no
#' offset, so there can be one more onset than offset, and in that case the
#' unmatched onset is the last. A pulse already in progress at the first
#' analysed sample is reported with its onset at that sample.
#'
#' [pd_dietrich2004()] is the one exception to the rule about a sample equal to
#' the threshold, as the comparisons of the paper it implements are not strict
#' ones. It is described there.
#'
#' @param wave A Wave object containing pulses
#' @param method Which method to use for pulse detection
#' @param ... Other arguments to pass to pulse detection function
#' @return The output of the pulse detection method used.
#' @export
#'
pulseDetection <- function(
  wave,
  method="simple",
  ...
) {
  #The methods below all read the left slot, so anything with more than one
  #channel goes through allChannels(), as dutyCycle() and rainfallDetection() do.
  #Stereo waves were otherwise analysed on the left channel alone, without saying
  #so, and a WaveMC has no left slot at all.
  if (.useAllChannels(wave)) {
    return(allChannels(
      wave,
      function(w) pulseDetection(w, method=method, ...),
      channel.param = NULL
    ))
  }
  .validateChoice(
    method, c("dietrich2004", "simple", "threshold"),
    msg="No valid method supplied."
  )
  if (method == "dietrich2004") {
    return(pd_dietrich2004(wave,...))
  }
  if (method == "simple") {
    return(pd_simple(wave,...))
  }
  return(pd_threshold(wave, ...))
}

#' Onsets and offsets of the runs of pulse samples in a logical vector
#'
#' Reports each run of TRUE values as the position of its first TRUE and the
#' position of the first value after it that is not TRUE, which is the package's
#' pulse boundary convention. A run reaching the end of the vector has an onset
#' and no offset, so onsets can hold one position more than offsets.
#'
#' @param above A logical vector, TRUE where a sample is part of a pulse.
#' @return A list of onsets and offsets, positions within above.
#' @noRd
.pulseRuns <- function(above) {
  n <- length(above)
  #above[-0] is the whole vector rather than nothing, so an empty wave would be
  #compared against a length one vector and recycled into one spurious pulse.
  if (n == 0) {
    return(list(onsets = integer(0), offsets = integer(0)))
  }
  #A sample that cannot be compared with the threshold is not part of a pulse,
  #rather than opening a pulse at one end of the comparison and closing it at the
  #other, which would leave an onset or an offset with nothing to pair with.
  above[is.na(above)] <- FALSE
  previous <- c(FALSE, above[-n])
  return(list(
    onsets = which(above & !previous),
    offsets = which(previous & !above)
  ))
}

#' Pair each onset with the offset that closes it
#'
#' Two independent scans of the same wave can report an onset with no offset, or
#' an offset with no onset, which leaves the two lists describing different
#' pulses. A pulse runs from an onset to the first offset after it; where several
#' onsets fall before that offset the scan found no boundary between them, so
#' they describe one pulse and the first of them opens it. An offset before the
#' first onset closes a pulse that was never opened and is discarded.
#'
#' @param onsets An increasing vector of candidate onset positions.
#' @param offsets An increasing vector of candidate offset positions.
#' @return A list of onsets and offsets of equal length, except where the last
#'   pulse has no offset.
#' @noRd
.pulsePairs <- function(onsets, offsets) {
  keptOnsets <- vector(mode="integer", length=length(onsets))
  keptOffsets <- vector(mode="integer", length=length(onsets))
  found <- 0
  closed <- 0
  i <- 1
  j <- 1
  while (i <= length(onsets)) {
    #An offset at or before this onset belongs to a pulse that has closed already.
    while (j <= length(offsets) && offsets[j] <= onsets[i]) {
      j <- j + 1
    }
    found <- found + 1
    keptOnsets[found] <- onsets[i]
    if (j > length(offsets)) {
      #Nothing closes this pulse, so it was still in progress at the last
      #analysed sample and is reported with an onset and no offset.
      break
    }
    keptOffsets[found] <- offsets[j]
    closed <- found
    while (i <= length(onsets) && onsets[i] < offsets[j]) {
      i <- i + 1
    }
  }
  return(list(
    onsets = keptOnsets[seq_len(found)],
    offsets = keptOffsets[seq_len(closed)]
  ))
}
