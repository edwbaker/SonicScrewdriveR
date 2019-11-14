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

  onsets <- vector(mode="logical", length=length(wave@left))
  tau <- 0
  p <- 0
  A <- 0
  for (t in (U/2+1):(length(wave@left)-U/2)) {
    if (e_u[t] >= F_1[t]) {
      if (tau > 0) {
        if (A==1) {
          if (e_u[t] >= F_a[t]) {
            p <- p+1
            onsets[t] <- TRUE
            A <- 0
          } else {
            if (tau > psi) {
              tau <- 0
            } else {
              tau <- tau+1
            }
          }
        }
      } else {
        tau <- tau + 1
      }
    } else {
      tau <- 0
      A <- 1
    }
  }
  offsets <- vector(mode="logical", length=length(wave@left))
  tau <- 0
  p <- 0
  A <- 0
  for (t in (U/2+1):(length(wave@left)-U/2)) {
    if (e_u[t] <= F_1[t]) {
      if (tau > 0) {
        if (A==1) {
          if (e_u[t] <= F_a[t]) {
            p <- p+1
            onsets[t] <- TRUE
            A <- 0
          } else {
            if (tau > psi) {
              tau <- 0
            } else {
              tau <- tau+1
            }
          }
        }
      } else {
        tau <- tau + 1
      }
    } else {
      tau <- 0
      A <- 1
    }
  }
  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    onsets=which(onsets == TRUE),
    offsets=which(offsets==TRUE),
    pulse_simple=e_u>F_1
  ))
}
