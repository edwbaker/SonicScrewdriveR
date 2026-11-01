cleanTZ <- function(tz) {
  if (substring(tz,1,3)=="UTC") {
    #The Etc/GMT zones take the sign of a POSIX TZ string, the reverse of the
    #ISO 8601 offset they look like: Etc/GMT-1 is an hour ahead of UTC, as UTC+1
    #is. The sign was copied across, which put UTC+1 two hours out.
    return(paste0("Etc/GMT", chartr("+-", "-+", substring(tz,4))))
  }
  #Anything that is not written as a UTC offset is already a timezone name, and
  #used to come back as NULL.
  return(tz)
}


