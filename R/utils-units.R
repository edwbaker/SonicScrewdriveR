#' Convert between units related by a constant factor
#'
#' Most of the convert2*() functions were the same chain of `if (input == "x")
#' return(value * k)` written out once per unit, once per direction. Holding the
#' factors in a table instead gives every direction from one set of numbers, so a
#' unit cannot be right in one function and wrong in its inverse.
#'
#' @param x The value to convert.
#' @param from The unit `x` is given in.
#' @param to The unit to convert to.
#' @param table A named numeric vector giving the size of each unit in whatever
#'   base unit the table is written in. Only the ratios matter.
#' @param what,fn,prep,msg Passed to `.validateChoice()` to word the error raised
#'   for an unknown `from`.
#' @return The converted value.
#' @keywords internal
#' @noRd
.convertLinear <- function(x, from, to, table, what="input", fn=NULL, prep="to", msg=NULL) {
  #Both ends are checked because which of them the user supplied varies: the
  #convert2*() functions take `from`, while wavelength() takes `to`.
  .validateChoice(from, names(table), what=what, fn=fn, prep=prep, msg=msg)
  .validateChoice(to, names(table), what=what, fn=fn, prep=prep, msg=msg)
  #Converting a unit to itself returns the value untouched. Multiplying and then
  #dividing by the same factor is not exact for a factor such as 0.1, and these
  #functions used to return their input unchanged in that case.
  if (from == to) {
    return(x)
  }
  return(x * table[[from]] / table[[to]])
}

#' Relative size of each pressure unit
#'
#' Only the ratios are used, so the table is written in dyne per square centimetre
#' to keep every entry a whole number. Converting through a fractional entry would
#' round where the arithmetic it replaced did not.
#'
#' @return A named numeric vector.
#' @noRd
.pressureMultipliers <- function() {
  return(c(Pa=10, kPa=10000, dyne_cm2=1))
}

#' Relative size of each angular unit
#'
#' Scaled so that the conversion works out as the value times 180 divided by pi,
#' rather than the value times a pre-rounded 180/pi.
#'
#' @return A named numeric vector.
#' @noRd
.angleMultipliers <- function() {
  return(c(degrees=pi, radians=180))
}

#' Relative size of each length unit used for wavelengths
#'
#' Written in centimetres so that both entries are whole numbers.
#'
#' @return A named numeric vector.
#' @noRd
.wavelengthMultipliers <- function() {
  return(c(m=100, cm=1))
}

#' Express a value in the largest unit that applies
#'
#' humanBytes() and humanTime() were the same findInterval() lookup written twice.
#' The unit tables and the pluralisation rule differ between them, so both are
#' arguments.
#'
#' @param S A numeric vector in the base unit of `multipliers`.
#' @param multipliers A named numeric vector of unit sizes, smallest first, not
#'   including the base unit itself.
#' @param base.name The name of the base unit, used for values below the first
#'   multiplier.
#' @param pluralise "all" to add an s to whichever unit is used, as time units
#'   take, or "base" to pluralise only the base unit, since kB is never kBs.
#' @param digits Number of decimal places to round to, or NULL for no rounding.
#' @return A character vector, one string per value of `S`. NA input gives NA.
#' @keywords internal
#' @noRd
.humanUnits <- function(S, multipliers, base.name, pluralise=c("all", "base"), digits=3) {
  pluralise <- match.arg(pluralise)

  #findInterval() gives zero for values smaller than the first unit, and otherwise
  #the position of the largest unit that applies.
  largest <- findInterval(S, multipliers)
  applies <- pmax(largest, 1)

  value <- ifelse(largest == 0, S, S / multipliers[applies])
  if (!is.null(digits)) {
    value <- round(value, digits)
  }

  if (pluralise == "all") {
    name <- ifelse(largest == 0, base.name, names(multipliers)[applies])
    name <- ifelse(value == 1, name, paste0(name, "s"))
  } else {
    name <- ifelse(
      largest == 0,
      ifelse(S == 1, base.name, paste0(base.name, "s")),
      names(multipliers)[applies]
    )
  }

  ret <- paste(value, name)
  ret[is.na(S)] <- NA_character_
  return(ret)
}
