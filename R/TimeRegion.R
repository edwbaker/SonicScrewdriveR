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

#' Specify a region with a file to analyse
#'
#' Specifies a time-bounded region to analyse.
#' @param input.unit Unit the from and to values are given in, one of
#'   "samples", "seconds", "minutes", "hours".
#' @param from Start time
#' @param to End time
#' @param unit Deprecated, use input.unit instead.
#' @return A TimeRegion object.
#' @export
region <- function(input.unit, from=0, to=Inf, unit) {
  if (!missing(unit)) {
    .deprecatedArg("unit", "input.unit", "region")
    input.unit <- unit
  }
  input.unit <- .validateUnit(input.unit, .wavePositionUnits(), "region")
  return(switch(
    input.unit,
    "samples" = .samples(from, to),
    "seconds" = .seconds(from, to),
    "minutes" = .minutes(from, to),
    "hours"   = .hours(from, to)
  ))
}

.samples <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "samples"
  return(t)
}

.seconds <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "seconds"
  return(t)
}

.minutes <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "minutes"
  return(t)
}

.hours <- function(from, to) {
  t <- new("TimeRegion")
  t@from <- from
  t@to <- to
  t@unit <- "hours"
  return(t)
}

.timeRegion2samples <- function(t, samp.rate) {
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
#' @return A Wave object containing only the samples within the time region.
setMethod("[", signature(x = "Wave", i = "TimeRegion"), function(x,i){
  if (inherits(i,"TimeRegion")) {
    tr <- .timeRegion2samples(i, x@samp.rate)
    if (is.infinite(tr[2])) {
      tr[2] <- length(x@left)
    }
    x@left <- x@left[tr[1]:tr[2]]
    if (x@stereo) {
      x@right <- x@right[tr[1]:tr[2]]
    }
    return(x)
  }
})
