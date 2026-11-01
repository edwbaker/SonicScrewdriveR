#' Pulse detection using Dietrich (2004)
#'
#' Detects pulses in a Wave using the method described in Dietrich et al (2004) <doi:10.1016/j.patcog.2004.04.004>.
#'
#' @details
#' Onsets and offsets follow the convention described under [pulseDetection()],
#' and the windowed energy displaces each boundary outward in the way described
#' under [pd_simple()].
#'
#' @param wave A Wave object
#' @param U Window length
#' @param gamma Gamma
#' @param alpha Alpha
#' @param scaling Scaling
#' @param V V Window length
#' @param psi Psi
#' @export
#' @return A list of input values plus the onset and offset times of pulses
#'
pd_dietrich2004 <- function(
  wave,
  U=120,
  gamma=0.05,
  alpha=1.4,
  scaling=32,
  V=480,
  psi=1
){
  e_u <- ste(wave, U=U)
  e_v <- ste(wave, U=V)

  n <- length(wave)
  #Only the samples both windows cover can be thresholded: ste() leaves a zero
  #where its window does not fit, and both e_u and e_v are read below. e_v was
  #zero across the half of the long window at each end, which collapsed F_1 and
  #F_a to theta there, and theta itself was taken from a floor those zeroes held
  #at zero however loud the recording was.
  t <- .steDefined(n, max(U, V))
  if (length(t) == 0) {
    theta <- NA_real_
  } else {
    energy <- e_u[t]
    theta <- min(energy) + gamma*(max(energy) - min(energy))
  }
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  #Nothing outside the analysed samples is a pulse, as the energy there is not a
  #windowed energy of this wave, so pulse_simple marks the same samples as
  #pd_simple()'s pulse does.
  pulse_simple <- vector(mode="logical", length=n)
  pulse_simple[t] <- e_u[t] > F_1[t]

  if (length(t) == 0) {
    onsets <- integer(0)
    offsets <- integer(0)
  } else {
    #A pulse already in progress at the first analysed sample is reported with
    #its onset there, as pd_threshold() reports one in progress at the first
    #sample of the wave. Suppressing it while the scan for offsets still reported
    #its end left one more offset than onset, so every later pulse was paired
    #with the previous pulse's offset and pulseStats() gave durations of -2505
    #and -1505 samples for three pulses of 400. A wave wholly above the threshold
    #reported neither an onset nor an offset.
    #The scan for offsets stays disarmed until it has seen a sample on the pulse
    #side of F_1, so a wave that begins in silence does not report an offset for
    #a pulse that never existed.
    #An energy exactly equal to a threshold is not part of a pulse, so the two
    #scans test opposite sides of the same line. Both sides were tested with a
    #non-strict comparison, which put such a sample on both sides at once: in
    #silence every sample is exactly equal to both thresholds, and arming the
    #scan for onsets would have reported the whole of a silent wave as one pulse.
    onsets <- t[.pd_dietrich_scan(e_u[t] > F_1[t], e_u[t] > F_a[t], psi, armed=TRUE)]
    offsets <- t[.pd_dietrich_scan(e_u[t] <= F_1[t], e_u[t] <= F_a[t], psi, armed=FALSE)]
    #The two scans are independent of one another, so they can still disagree
    #about how many pulses the wave holds: a run that crosses F_1 without ever
    #reaching F_a gives no onset while the silence after it gives an offset, and
    #a dip below F_1 too short for the counter to test gives a second onset
    #without an offset between the two. Pairing each onset with the offset that
    #closes it keeps the two lists describing the same pulses.
    paired <- .pulsePairs(onsets, offsets)
    onsets <- paired$onsets
    offsets <- paired$offsets
  }

  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    onsets = onsets,
    offsets = offsets,
    pulse_simple = pulse_simple
  ))
}

#' Scan for pulse boundaries using the Dietrich (2004) state machine
#'
#' The state machine in Dietrich (2004) walks the wave one sample at a time, but it
#' only ever reacts to runs of consecutive samples on the same side of the F_1
#' threshold, so it can be evaluated a run at a time instead.
#'
#' Within a run the machine is only armed once a sample on the other side of F_1 has
#' been seen, so the first run is skipped when it starts at the first analysed
#' sample, unless the machine is armed to begin with. The counter tau then advances
#' independently of F_a on a cycle of floor(psi)+2 samples, and F_a is not tested on
#' the sample where tau has just been reset. The machine disarms as soon as it
#' fires, so each run yields at most one boundary.
#'
#' @param b Logical vector, TRUE where the energy is on the pulse side of F_1.
#' @param a Logical vector, TRUE where the energy is on the pulse side of F_a.
#' @param psi Psi
#' @param armed Whether the machine starts armed, so that a run already in
#'   progress at the first analysed sample can yield a boundary.
#' @return A vector of positions within b at which a boundary is detected.
#' @noRd
.pd_dietrich_scan <- function(b, a, psi, armed=FALSE) {
  runs <- rle(b)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  cycle <- max(2, floor(psi) + 2)

  if (armed) {
    candidates <- which(runs$values)
  } else {
    candidates <- which(runs$values & starts > 1)
  }
  hits <- vector(mode="integer", length=length(candidates))
  found <- 0
  for (i in candidates) {
    k <- seq_len(runs$lengths[i])
    testable <- k %% cycle != 1
    hit <- which(testable & a[starts[i] + k - 1])
    if (length(hit) > 0) {
      found <- found + 1
      hits[found] <- starts[i] + hit[1] - 1
    }
  }
  return(hits[seq_len(found)])
}
