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
  theta <- min(e_u) + gamma*(max(e_u) - min(e_u))
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling
  pulse <- e_u > F_a
  
  return(list(
    theta = theta,
    F_1 = F_1,
    e_u = e_u,
    pulse = pulse,
    onsets = which(diff(pulse) == 1),
    offsets = which(diff(pulse) == -1)
  ))
}