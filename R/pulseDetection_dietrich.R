#' Pulse detection using Dietrich (2004)
#'
#' Detects pulses in a Wave using the method described in Dietrich et al (2004) <doi:10.1016/j.patcog.2004.04.004>.
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
  theta <- min(e_u) + gamma*(max(e_u) - min(e_u))
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  n <- length(wave)
  before <- floor(U/2)
  after <- ceiling(U/2)

  if (n - after < before + 1) {
    t <- integer(0)
    onsets <- integer(0)
    offsets <- integer(0)
  } else {
    t <- (before + 1):(n - after)
    onsets <- t[.pd_dietrich_scan(e_u[t] >= F_1[t], e_u[t] >= F_a[t], psi)]
    offsets <- t[.pd_dietrich_scan(e_u[t] <= F_1[t], e_u[t] <= F_a[t], psi)]
  }

  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    onsets = onsets,
    offsets = offsets,
    pulse_simple = e_u > F_1
  ))
}

#' Scan for pulse boundaries using the Dietrich (2004) state machine
#'
#' The state machine in Dietrich (2004) walks the wave one sample at a time, but it
#' only ever reacts to runs of consecutive samples on the same side of the F_1
#' threshold, so it can be evaluated a run at a time instead.
#'
#' Within a run the machine is only armed once a sample on the other side of F_1 has
#' been seen, so the first run is skipped when it starts at the first analysed sample.
#' The counter tau then advances independently of F_a on a cycle of floor(psi)+2
#' samples, and F_a is not tested on the sample where tau has just been reset. The
#' machine disarms as soon as it fires, so each run yields at most one boundary.
#'
#' @param b Logical vector, TRUE where the energy is on the pulse side of F_1.
#' @param a Logical vector, TRUE where the energy is on the pulse side of F_a.
#' @param psi Psi
#' @return A vector of positions within b at which a boundary is detected.
#' @noRd
.pd_dietrich_scan <- function(b, a, psi) {
  runs <- rle(b)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  cycle <- max(2, floor(psi) + 2)

  candidates <- which(runs$values & starts > 1)
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
