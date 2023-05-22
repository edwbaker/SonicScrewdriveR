pulseStats <- function(pulseInfo) {
  pulseDurations <- pulseInfo$offsets - pulseInfo$onsets
  l <- length(pulseInfo$offsets)
  pulseDistances <- pulseInfo$onsets[2:l] - pulseInfo$offsets[1:(l-1)]
  return(
    list(
      "durations" = pulseDurations,
      "distances" = pulseDistances
    )
  )
}