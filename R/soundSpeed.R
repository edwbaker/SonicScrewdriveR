soundSpeed <- function(medium="air", 
                       method=NULL, 
                       temperature=NULL) {
  if (is.null(method)) {
    if (is.null(temperature)) {
      return(soundSpeed_vague(medium=medium))
    }
  }
}

soundSpeed_vague <- function(medium="air") {
  if (medium == "air") {
    return (343)
  }
  stop("No sound speed data for medium: ", medium)
}