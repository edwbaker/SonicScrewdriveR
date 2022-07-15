#' Convert time to seconds
#'
#' Converts time measurements into seconds
#'
#' @param T The time value to convert
#' @param input The unit of time to convert, allowed values are "minutes", "hours", "days", "years".
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
  if (input == "years") {
    return(T*60*60*24*365)
  }
  stop(paste("Unknown input to convert2seconds: ",input))
}

convert2fractionalCircle <- function(T, input="HHMM") {
  if (input == "HHMM") {
    T<- stri_pad(T, 4, "left", 0)
    T <- pi - 2*pi*(as.numeric(substr(T,1,2))*60 + as.numeric(substr(T,3,4))) / 1440
    T[is.na(T)] <- 0
    return(T)
  }
}
