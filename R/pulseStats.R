pulseStats <- function(pulseInfo) {
  #A recording can end part way through a pulse, leaving one more onset than
  #offset, and 2:l counted backwards when there were fewer than two pulses.
  paired <- min(length(pulseInfo$onsets), length(pulseInfo$offsets))
  pulseDurations <- pulseInfo$offsets[seq_len(paired)] - pulseInfo$onsets[seq_len(paired)]

  gaps <- seq_len(max(0, paired - 1))
  pulseDistances <- pulseInfo$onsets[gaps + 1] - pulseInfo$offsets[gaps]
  return(
    list(
      "durations" = pulseDurations,
      "distances" = pulseDistances
    )
  )
}