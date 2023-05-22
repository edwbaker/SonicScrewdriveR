cleanTZ <- function(tz) {
  if (substring(tz,1,3)=="UTC") {
    return(paste0("Etc/GMT", substring(tz,4)))
  }
}


