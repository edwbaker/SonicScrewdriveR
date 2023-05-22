#' The radar equation
#'
#' Calculates the power returned from an echolocation pulse
#'
#' @param P_t Power transmitted (from sender)
#' @param area Effective cross-sectional area of the target
#' @param r Range of the target
#' @param G_t Transmitter gain
#' @param G_r Receiver gain
#' @param wl Wavelength (use only with G_r and G_t)
#' @export
#' @return The received power
#' @examples
#' P_r(12, 20, 0.05)
#' P_r(12, 20, 0.05, G_t=1.2, G_r=1.5, wl=0.045)
#'
P_r <- function(P_t, r, area, G_t=1, G_r=1, wl=1) {
  P_r <- (P_t * area * G_t * G_r * wl^2) / ( (4*pi)^3 * r^4)
  return (P_r)
}

#' Radar range
#'
#' Calculates the distance of an object based on the round trip time of an
#' echolocation pulse
#'
#' @param t Time in seconds
#' @param c Speed of sound in transmission medium m/s (by default air)
#' @export
#' @return Distance to object
#' @examples
#' radar.range(2)
#' radar.range(2, c=343)
#' radar.range(2, c=soundSpeedMedium("sea water"))
#'
radar.range <- function(t, c=soundSpeedMedium(medium="air")) {
  range <- t * c /2
  return(range)
}
