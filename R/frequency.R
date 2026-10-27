#' Get the frequency from wavelength and speed of sound
#'
#' Calculates the frequency of a sound wave given the wavelength and speed of sound in that medium.
#'
#' @param wl Wavelength
#' @param s Speed of sound (defaults to the speed of sound in air)
#' @return Frequency of the sound in Hertz
#' @examples
#' f <- frequencySound(wl=100, s=343)
#' @export
#'
frequencySound <- function(wl, s=soundSpeed(medium="air")) {
  f <- validateSpeed(s) / validateWavelength(wl)
  return(validateFreq(f))
}

#' Calculate the natural frequency
#'
#' Calculates the natural frequency given the inductance, capacitance and resistance. In the acoustic case
#' the inductance is inertia or mass, the capacitance is elasticity (bulk modulus) and resistance is composed of air
#' resistance and related quantities. All units are SI.
#'
#' For isothermal compression, the bulk modulus is equal to the pressure. The default value of C therefore is the IUPAC
#' standard pressure, which in SI units is 1e5 Pascals.
#'
#' A system in which the damping term exceeds the stiffness term (R^2/(4L^2) > 1/(LC)) is overdamped: it returns to
#' rest without oscillating, and so has no real natural frequency. This raises an error rather than returning NaN.
#'
#' @param L Inductance
#' @param C Capacitance, by default the IUPAC standard pressure of 1e5 Pascals.
#' @param R Resistance
#' @importFrom utils data
#' @examples
#' naturalFrequency(L=20,R=0.01)
#' naturalFrequency(L=20,C=1/4,R=0.5)
#' @return The natural frequency, in Hertz.
#' @export
#'
naturalFrequency <- function(L, C=1e5, R) {
  discriminant <- (1/(L*C)) - R^2/(4*L^2)
  if (any(discriminant < 0)) {
    stop("System is overdamped (R^2/(4L^2) > 1/(LC)), so has no real natural frequency. Note that C is in SI units, the default being the IUPAC standard pressure of 1e5 Pascals.")
  }
  F_nat <- sqrt(discriminant) / (2 * pi)
  return(validateFreq(F_nat))
}

#' Calculate the resonant frequency
#'
#' Calculates the resonant frequency given the inductance and capacitance. In the acoustic case
#' the inductance is inertia or mass, the capacitance is elasticity (bulk modulus) and resistance is composed of air
#' resistance and related quantities. All units are SI.
#'
#' For isothermal compression, the bulk modulus is equal to the pressure. The default value of C therefore is the IUPAC
#' standard pressure, which in SI units is 1e5 Pascals.
#'
#' @param L Inductance
#' @param C Capacitance, by default the IUPAC standard pressure of 1e5 Pascals.
#' @importFrom utils data
#' @examples
#' f <- resonantFrequency(L=1)
#' @return The resonant frequency, in Hertz.
#' @export
#'
resonantFrequency <- function(L, C=1e5) {
  F_res <- naturalFrequency(L,C,0)
  return(validateFreq(F_res))
}
