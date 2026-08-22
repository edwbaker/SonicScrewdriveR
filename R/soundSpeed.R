#' Calculate the speed of sound in a medium
#'
#' Given sufficient parameters (i.e. wavelength and frequency, bulk modulus and density) this
#' function calculates the speed of sound.
#'
#' @param medium Propagation medium (e.g. "air"), or "all" to return a list of all available media.
#' @param method Use a specific method to calculate the speed of sound (see Details).
#' @param wl Wavelength
#' @param f Frequency
#' @param bulkModulus Bulk modulus
#' @param density Density
#' @param ... Additional parameters passed to the method.
#' @details
#' The speed of sound can be calculated using the following methods:
#' * **cramer** Uses the method described in \insertCite{cramer1993;textual}{sonicscrewdriver}.
#'   Additional parameters are:
#'   * temp Temperature
#'   * temp.unit Temperature unit
#'   * pressure Pressure
#'   * pressure.unit Pressure unit
#'   * RH Relative humidity
#'   * MoleFracCO2 Mole fraction of CO2
#' * **seewave** Delegates the calculation of the speed of sound in air to the
#'   package `seewave` \insertCite{seewave2008}{sonicscrewdriver}. This calculation is
#'.  performed as \eqn{\text{speed} = 331.4 + 0.6 \times \text{temp}}.
#'   Additional parameters are:
#'   * temp Temperature
#' @references
#'   \insertAllCited{}
#' @examples
#' soundSpeed(medium="air")
#' soundSpeed(medium="sea water")
#'
#' soundSpeed(method="cramer", temp=14, pressure=3, RH=10)
#' soundSpeed(method="cramer", temp=14, temp.unit="C", pressure=3, pressure.unit="kPa", RH=10)
#'
#' t <- 1:30
#' s <- lapply(t, function(x){soundSpeed(method="cramer", temp=x)})
#' @return The speed of sound in the medium, in metres per second.
#' @export
soundSpeed <- function(medium=NULL, method=NULL, wl=NULL, f=NULL, bulkModulus=NULL, density=NULL, ...) {
  # If method specified use it
  if (!is.null(method)) {
    if (method=="cramer") {
      s <- .soundSpeed_cramer1993(...)
      return(s)
    }
    if (method=="seewave") {
      # Use dummy value for f, not used in calculation
      if (!"temp" %in% names(list(...))) {
        stop("Temperature must be specified.")
      }
      return(seewave::wasp(f=1, t=list(...)$temp, medium="air")$c)
    }
    #Without this an unknown method fell out of the block and the default
    #below returned the speed of sound in air as though nothing were wrong.
    stop(paste("Unknown method for soundSpeed:", method))
  }
  # If all arguments are null set default
  if (all(is.null(medium), is.null(wl), is.null(f), is.null(bulkModulus), is.null(density))) {
    medium <- "air"
  }
  if (!is.null(medium)) {
    s <- .soundSpeedMedium(medium)
    return(s)
  }
  if (!is.null(wl) & !is.null(f)) {
    s <- validateWavelength(wl) * validateFreq(f)
    return(s)
  }
  if (!is.null(bulkModulus) & !is.null(density)) {
    s <- sqrt(validateBulkModulus(bulkModulus)/validateDensity(density))
    return(s)
  }
  stop("Insufficient arguments to calculate the speed of sound.")
}

#' Get the speed of sound in a medium
#'
#' Provides typical values of the speed of sound in a given medium (air,
#' sea water, freshwater).
#'
#' @param medium Propagation medium (default is "air"). "all" gives a list of all
#'   available media.
#' @return Typical value of the speed of sound in m/s for the medium.
#' @keywords internal
#' @noRd
.soundSpeedMedium <- function(medium="air") {
  names  <- c("air", "sea water", "freshwater", "helium", "hydrogen", "liquid helium", "mercury", "aluminium", "lead", "steel")
  values <- c(343, 1500, 1430, 999, 1330, 211, 1451, 6420, 1960, 5941)
  if (medium=="all") {
    #cbind() of a character and a numeric vector gives a character matrix, which
    #turned every speed into a string.
    return(data.frame(names = names, values = values, stringsAsFactors = FALSE))
  }
  if (medium %in% names) {
    return(values[which(names==medium)])
  }
  stop("No sound speed data for medium: ", medium)
}

#' Speed of sound in air using Cramer (1993)
#'
#' Calculate the speed of sound in air using the method described in Cramer (1993) <doi:10.1121/1.405827>
#'
#' @param temp Temperature
#' @param temp.unit Temperature unit
#' @param pressure Pressure
#' @param pressure.unit Pressure unit
#' @param RH Relative humidity
#' @param MoleFracCO2 Mole fraction of CO2. The default is the nominal composition
#'   used by Cramer, 400 ppm.
#' @details
#' Cramer's expression is valid for temperatures from 0 to 30 degrees Celsius,
#' pressures from 75 to 102 kPa, a mole fraction of water vapour up to 0.06, and a
#' mole fraction of CO2 up to 0.01. Values outside those ranges are extrapolations.
#' @keywords internal
#' @noRd
#' @return Numeric value of the speed of sound in m/s
#' @examples
#' .soundSpeed_cramer1993(14, pressure=3, RH=10)
#' .soundSpeed_cramer1993(14, temp.unit="C", pressure=3, pressure.unit="kPa", RH=10)
.soundSpeed_cramer1993 <- function(temp,
                                  temp.unit = "C",
                                  pressure=100,
                                  pressure.unit = "kPa",
                                  RH=50,
                                  MoleFracCO2=400e-6) {
  validateRH(RH)
  K <- convert2Kelvin(temp, temp.unit)
  celsius <- convert2Celsius(temp, temp.unit)
  P <- convert2Pascals(pressure, pressure.unit)

  #Davis, Metrologia, 29, p67, 1992
  ENH <- pi * 10^-8 * P + 1.00062 + celsius^2 * 5.6 * 10^-7

  PSV1 <- K^2*1.2378847*10^-5 - 1.9121316*10^-2 * K
  PSV2 <- 33.93711047-6.3431645*10^3 / K
  PSV  <- exp(1)^PSV1 * exp(1)^PSV2

  MolConH20  <- RH*ENH*PSV/P
  MoleFracH20 <- MolConH20/100

  C1 <- 0.603055*celsius + 331.5024- celsius^2*5.28*10^-4 + (0.1495874*celsius + 51.471935 - celsius^2*7.82*10^-4)*MoleFracH20
  C2 <- (-1.82*10^-7+3.73*10^-8*celsius-celsius^2*2.93*10^-10)*P+(-85.20931-0.228525*celsius+ celsius^2*5.91*10^-5)*MoleFracCO2
  C3 = MoleFracH20^2*2.835149 + P^2*2.15*10^-13 - MoleFracCO2^2*29.179762 - 4.86*10^-4*MoleFracH20*P*MoleFracCO2
  C = C1+C2-C3
  return(C)
}
