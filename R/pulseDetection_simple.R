#' Simplified pulse detection using Dietrich (2004)
#'
#' Detects pulses in a Wave.
#' 
#' @param wave A Wave object
#' @param U Window length
#' @param gamma Gamma
#' @param alpha Alpha
#' @param scaling Scaling
#' @param V V Window length
#' @param psi Psi
#' @return A list of input values plus the onset and offset times of pulses.
#' @export
#'
pd_simple <- function(
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
  #A wave with no samples gives no short term energy to threshold, and min() and
  #max() of nothing warn and return Inf and -Inf, making theta NaN. There is
  #nothing to detect, so theta is undefined and the empty comparisons below carry
  #that through to an empty result, as pd_threshold() already does.
  if (length(e_u) == 0) {
    theta <- NA_real_
  } else {
    theta <- min(e_u) + gamma*(max(e_u) - min(e_u))
  }
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling
  pulse <- e_u > F_a
  
  #F_a is the threshold the pulses were found with, so it is returned alongside
  #F_1. Returning only F_1 meant the threshold given back did not reproduce the
  #pulses given back. The names match pd_dietrich2004().
  return(list(
    theta = theta,
    F_a = F_a,
    F_1 = F_1,
    e_u = e_u,
    e_v = e_v,
    pulse = pulse,
    onsets = which(diff(pulse) == 1),
    offsets = which(diff(pulse) == -1)
  ))
}