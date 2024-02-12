#' An S4 class to represent a TimeRegion within a Wave object.
#'
#' @slot from Start position
#' @slot to End position
#' @slot unit Time unit (one of seconds, minutes, hours)
setClass(
  "TimeRegion",
  slots=list(
    from="numeric",
    to="numeric",
    unit="character"
  ),
  prototype = list(
    from = 1,
    to = 100,
    unit = "samples"
  )
)

seconds <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "seconds"
  return(t)
}

minutes <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "minutes"
  return(t)
}

hours <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "hours"
  return(t)
}

timeRegion2samples <- function(t, samp.rate) {
  if (t@unit == "samples") {
    return(c(t@from,t@to))
  }
  if (t@unit =="seconds") {
    return(c(max(1,(t@from*samp.rate)),(t@to*samp.rate)))
  }
  if (t@unit =="minutes") {
    return(c(max(1,(t@from*samp.rate)),(t@to*samp.rate)*60))
}
  if (t@unit =="hours") {
    return(c(max(1,(t@from*samp.rate)),(t@to*samp.rate)*3600))
  }
}

#' Allow subsetting a Wave object with a TimeRegion
#' @param x Wave Object
#' @param i TimeRegion object
setMethod("[", signature(x = "Wave", i = "TimeRegion"), function(x,i){
  if (inherits(i,"TimeRegion")) {
    tr <- timeRegion2samples(i, x@samp.rate)
    x@left <- x@left[tr[1]:tr[2]]
    if (x@stereo) {
      x@right <- x@right[tr[1]:tr[2]]
    }
    return(x)
  }
})
