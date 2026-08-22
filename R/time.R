cleanTZ <- function(tz) {
  if (substring(tz,1,3)=="UTC") {
    return(paste0("Etc/GMT", substring(tz,4)))
  }
  #Anything that is not written as a UTC offset is already a timezone name, and
  #used to come back as NULL.
  return(tz)
}


