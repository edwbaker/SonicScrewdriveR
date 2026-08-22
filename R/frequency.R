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
#' standard pressure.
#'
#' @param L Inductance
#' @param C Capacitance, by default IUPAC standard pressure.
#' @param R Resistance
#' @importFrom utils data
#' @examples
#' naturalFrequency(L=20,R=0.5)
#' naturalFrequency(L=20,C=1/4,R=0.5)
#' @return The natural frequency, in Hertz.
#' @export
#'
naturalFrequency <- function(L, C=NULL, R) {
  #A NULL sentinel rather than a string, which could not be compared against a
  #vector of capacitances without raising an error about the length of the
  #condition, so these could not be vectorised.
  if (is.null(C)) {
    C <- 100
  }
  F_nat <- sqrt((1/(L*C)) - R^2/(4*L^2)) / (2 * pi)
  return(validateFreq(F_nat))
}

#' Calculate the resonant frequency
#'
#' Calculates the resonant frequency given the inductance and capacitance. In the acoustic case
#' the inductance is inertia or mass, the capacitance is elasticity (bulk modulus) and resistance is composed of air
#' resistance and related quantities. All units are SI.
#'
#' For isothermal compression, the bulk modulus is equal to the pressure. The default value of C therefore is the IUPAC
#' standard pressure.
#'
#' @param L Inductance
#' @param C Capacitance, if NULL (the default) the IUPAC standard pressure is used.
#' @importFrom utils data
#' @examples
#' f <- resonantFrequency(L=1)
#' @return The resonant frequency, in Hertz.
#' @export
#'
resonantFrequency <- function(L, C=NULL) {
  F_res <- naturalFrequency(L,C,0)
  return(validateFreq(F_res))
}
