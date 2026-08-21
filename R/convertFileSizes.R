#' Convert file sizes to bytes
#'
#' Converts file size measurements into bytes. Both decimal units, which are powers
#' of 1000, and binary units, which are powers of 1024, are accepted.
#'
#' @param S The value to convert
#' @param input The unit to convert, one of those given by fileSizeUnits(), or
#'   "bits" or "bytes".
#' @export
#' @return The numeric value in bytes
#' @examples
#' convert2bytes(8, input="bits")
#' convert2bytes(1, input="kB")
#' convert2bytes(1, input="KiB")
#'
convert2bytes <- function(S, input="bits") {
  units <- c(
    bits = 1/8,
    bytes = 1,
    .fileSizeMultipliers("decimal"),
    .fileSizeMultipliers("binary")
  )
  if (!input %in% names(units)) {
    stop(paste("Unknown input to convert2bytes:", input))
  }
  return(S * units[[input]])
}

#' Units used for file sizes
#'
#' The units understood by convert2bytes() and produced by humanBytes(). Decimal
#' units are powers of 1000 and binary units are powers of 1024.
#'
#' @param units One of "decimal" or "binary"
#' @export
#' @return A character vector of unit names, from smallest to largest.
#' @examples
#' fileSizeUnits()
#' fileSizeUnits("binary")
#'
fileSizeUnits <- function(units="decimal") {
  return(names(.fileSizeMultipliers(units)))
}

#' Number of bytes in each file size unit
#'
#' @param units One of "decimal" or "binary"
#' @return A named numeric vector of the number of bytes in each unit.
#' @noRd
.fileSizeMultipliers <- function(units="decimal") {
  if (units == "decimal") {
    ret <- 1000^(1:6)
    names(ret) <- c("kB", "MB", "GB", "TB", "PB", "EB")
    return(ret)
  }
  if (units == "binary") {
    ret <- 1024^(1:6)
    names(ret) <- c("KiB", "MiB", "GiB", "TiB", "PiB", "EiB")
    return(ret)
  }
  stop(paste("Unknown units for file sizes:", units))
}

#' Converts bytes in human readable form
#'
#' Given an input of bytes calculates the result in a sensible output unit (e.g.
#' MB, GB, PB). Decimal units are powers of 1000, and binary units powers of 1024.
#'
#' @param S Number of bytes. A vector may be given.
#' @param units One of "decimal", giving units such as kB, or "binary", giving units
#'   such as KiB.
#' @param digits Number of decimal places to round to, or NULL for no rounding.
#' @return String in human readable format, one for each value of S.
#' @export
#' @examples
#' humanBytes(1500)
#' humanBytes(1500, units="binary")
#' humanBytes(c(1, 1024, 1e6))
#'
humanBytes <- function(S, units="decimal", digits=3) {
  multipliers <- .fileSizeMultipliers(units)

  #findInterval() gives zero for sizes smaller than the first unit, and otherwise
  #the position of the largest unit that applies.
  unit <- findInterval(S, multipliers)
  applies <- pmax(unit, 1)

  value <- ifelse(unit == 0, S, S / multipliers[applies])
  if (!is.null(digits)) {
    value <- round(value, digits)
  }
  name <- ifelse(unit == 0, ifelse(S == 1, "byte", "bytes"), names(multipliers)[applies])

  ret <- paste(value, name)
  ret[is.na(S)] <- NA_character_
  return(ret)
}
