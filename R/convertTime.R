#' Convert time to seconds
#'
#' Converts time measurements into seconds
#'
#' @param T The time value to convert
#' @param input The unit of time to convert, allowed values are "minutes", "hours", "days".
#' @export
#' @return The numeric value in seconds
#'
convert2seconds <- function(T, input="minutes") {
  if (input == "seconds") {
    return(T)
  }
  if (input == "minutes") {
    return(T*60)
  }
  if (input == "hours") {
    return(T*60*60)
  }
  if (input == "days") {
    return(T*60*60*24)
  }
  stop(paste("Unknown input to convert2seconds:",input))
}
