#' Convert bits to bytes
#'
#' Converts time measurements into seconds
#'
#' @param S The value to convert
#' @param input The unit  to convert, allowed values are "bits", "kB", "MB", "GB"
#' @export
#' @return The numeric value in seconds
#'
convert2bytes <- function(S, input="bits") {
  if (input == "bits") {
    return(S/8)
  }
  if (input =="bytes") {
    return(S)
  }
  if (input == "kB") {
    return(S*1e+3)
  }
  if (input == "MB") {
    return(S*1e+6)
  }
  if (input == "GB") {
    return(S*1e+9)
  }
  stop(paste("Unknown input to convert2bytes:",input))
}

#' Converts bytes in human readable form
#'
#' Given an input of bytes calculates the result in a sensible output unit (e.g.
#' MB, GB, PB).
#'
#' @param S Number of bytes
#' @return String in human readable format
#' @export
#'
humanBytes <- function(S) {
  if (S > 1e+18) {
    S <- S/1e+18
    return(paste(S,"EB"))
  }
  if (S > 1e+15) {
    S <- S/1e+15
    return(paste(S, "PB"))
  }
  if (S > 1e+12) {
    S <- S/1e+12
    return(paste(S, "TB"))
  }
  if (S > 1e+9) {
    S <- S/1e+9
    return(paste(S, "GB"))
  }
  if (S > 1e+6) {
    S <- S/1e+6
    return(paste(S, "MB"))
  }
  if (S > 1e+3) {
    S <- S/1e+3
    return(paste(S, "kB"))
  }
}
