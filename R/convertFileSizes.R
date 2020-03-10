#' Convert bits to bytes
#'
#' Converts time measurements into seconds
#'
#' @param S The value to convert
#' @param input The unit  to convert, allowed values are "bits"
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
  stop(paste("Unknown input to convert2bytes:",input))
}

printHumanBytes <- function(S, input="bytes") {
  S <- convert2bytes(S, input=input)
  if (S > 1e+18) {
    S <- S/1e+18
    print(paste(S,"EB"))
  }
  if (S > 1e+15) {
    S <- S/1e+15
    print(paste(S, "PB"))
  }
  if (S > 1e+12) {
    S <- S/1e+12
    print(paste(S, "TB"))
  }
  if (S > 1e+9) {
    S <- S/1e+9
    print(paste(S, "GB"))
  }
  if (S > 1e+6) {
    S <- S/1e+6
    print(paste(S, "MB"))
  }
  if (S > 1e+3) {
    S <- S/1e+3
    print(paste(S, "kB"))
  }
}
