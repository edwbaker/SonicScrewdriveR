#' Speed of sound in air using Cramer (1993)
#'
#' Calculate the speed of sound in air using the method described in Cramer (1993) <doi:10.1121/1.405827>
#'
#' @param temp Temperature
#' @param temp.unit Temperature unit
#' @param pressure Pressure
#' @param pressure.unit Pressure unit
#' @param RH Relative humidity
#' @param MoleFracCO2 Mole fraction of CO2
#' @export
#' @return Numeric value of the speed of sound in m/s
#' @examples
#' soundSpeed_cramer1993(14, pressure=3, RH=10)
#' soundSpeed_cramer1993(14, temp.unit="C", pressure=3, pressure.unit="kPa", RH=10)
#'
soundSpeed_cramer1993 <- function(temp,
                                  temp.unit = "C",
                                  pressure,
                                  pressure.unit = "kPa",
                                  RH,
                                  MoleFracCO2=400^-6) {
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
  C2 <- (-1.82*10^-7+3.73*10^-8*T-celsius^2*2.93*10^-10)*P+(-85.20931-0.228525*celsius+ celsius^2*5.91*10^-5)*MoleFracCO2
  C3 = MoleFracH20^2*2.835149 + P^2*2.15*10^-13 - MoleFracCO2^2*29.179762 - 4.86*10^-4*MoleFracH20*P*MoleFracCO2
  C = C1+C2-C3
  return(C)
}
