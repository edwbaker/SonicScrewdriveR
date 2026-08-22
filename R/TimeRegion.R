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
#' @param unit Unit of time (one of samples, seconds, minutes, hours)
#' @param from Start time
#' @param to End time
#' @return A TimeRegion object.
#' @export
region <- function(unit, from=0, to=Inf) {
  .validateChoice(
    unit, c("samples", "seconds", "minutes", "hours"),
    msg="Unit must be one of samples, seconds, minutes, hours"
  )
  return(new("TimeRegion", from=from, to=to, unit=unit))
}

.timeRegion2samples <- function(t, samp.rate) {
  #One multiplier per unit, applied to both ends. Applying it to only the end of
  #the region put the start of a region given in minutes or hours at the sample
  #for that many seconds.
  multiplier <- switch(t@unit,
    samples = 1,
    seconds = samp.rate,
    minutes = samp.rate * 60,
    hours = samp.rate * 3600,
    stop(paste("Unknown unit for TimeRegion:", t@unit))
  )
  return(c(max(1, t@from * multiplier), t@to * multiplier))
}

#' Allow subsetting a Wave object with a TimeRegion
#' @param x Wave Object
#' @param i TimeRegion object
#' @return A Wave object containing only the samples within the time region.
setMethod("[", signature(x = "Wave", i = "TimeRegion"), function(x,i){
  if (inherits(i,"TimeRegion")) {
    tr <- .timeRegion2samples(i, x@samp.rate)
    #Clamped either way. Only an infinite end used to be brought back to the
    #length of the wave, so a finite region reaching past it gave a wave padded
    #with NA rather than one ending where the audio does.
    tr[1] <- max(1, tr[1])
    tr[2] <- min(tr[2], length(x@left))
    if (tr[2] < tr[1]) {
      stop("Time region begins after the end of the wave.")
    }
    x@left <- x@left[tr[1]:tr[2]]
    if (x@stereo) {
      x@right <- x@right[tr[1]:tr[2]]
    }
    return(x)
  }
})
