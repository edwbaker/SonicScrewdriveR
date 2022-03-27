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
    return(t@from:t@to)
  }
  if (t@unit =="seconds") {
    return(max(1,(t@from*samp.rate)):(t@to*samp.rate))
  }
  if (t@unit =="minutes") {
    return((max(1,(t@from*samp.rate)):(t@to*samp.rate))*60)
  }
  if (t@unit =="hours") {
    return((max(1,(t@from*samp.rate)):(t@to*samp.rate))*3600)
  }
}

setMethod("[", signature(x = "Wave", i = "TimeRegion"), function(x,i,j,...,drop=FALSE){
  if (class(i)=="TimeRegion") {
    x@left <- x@left[timeRegion2samples(i, x@samp.rate)]
    if (x@stereo) {
      x@right <- x@right[timeRegion2samples(i, x@samp.rate)]
    }
    return(x)
  }
})
