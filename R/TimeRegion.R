#' The units a TimeRegion may be given in
#'
#' Held once, so that the class and [region()] cannot disagree about them.
#' @return A character vector of the allowed units.
#' @keywords internal
#' @noRd
.timeRegionUnits <- function() {
  return(c("samples", "seconds", "minutes", "hours"))
}

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
  ),
  #Without a validity method new("TimeRegion", ...) accepted anything the slot
  #types allowed, and region() checked only its time.unit. A region running
  #backwards survived until `[` reported "Time region begins after the end of
  #the wave.", which is false and points at the wave rather than at the region;
  #an unknown unit survived until .timeRegion2samples() reached its stop().
  validity = function(object) {
    problems <- character(0)
    if (length(object@unit) != 1 || !object@unit %in% .timeRegionUnits()) {
      problems <- c(problems, paste0(
        "unit must be one of ", paste(.timeRegionUnits(), collapse=", "),
        ", not ", paste(object@unit, collapse=", ")
      ))
    }
    if (length(object@from) != 1 || is.na(object@from)) {
      problems <- c(problems, "from must be a single number")
    } else if (object@from < 0) {
      problems <- c(problems, paste0("from cannot be negative, and is ", object@from))
    }
    if (length(object@to) != 1 || is.na(object@to)) {
      problems <- c(problems, "to must be a single number")
    }
    #Both ends are checked for being numbers first, so this cannot be reached
    #with an NA on either side.
    if (!length(problems) && object@to < object@from) {
      problems <- c(problems, paste0(
        "a region cannot end before it begins, and runs from ",
        object@from, " to ", object@to
      ))
    }
    if (length(problems)) {
      return(problems)
    }
    return(TRUE)
  }
)

#' Specify a region with a file to analyse
#'
#' Specifies a time-bounded region to analyse.
#' @param time.unit Unit the from and to values are given in (one of samples,
#'   seconds, minutes, hours)
#' @param from Start time
#' @param to End time
#' @param unit Deprecated, use time.unit instead.
#' @return A TimeRegion object.
#' @export
region <- function(time.unit, from=0, to=Inf, unit) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(unit="time.unit"), "region"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  .validateChoice(
    time.unit, .timeRegionUnits(),
    msg="Unit must be one of samples, seconds, minutes, hours"
  )
  return(new("TimeRegion", from=from, to=to, unit=time.unit))
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
  if (t@unit == "samples") {
    return(c(max(1, t@from), t@to))
  }
  #A region in time runs from its start up to its end, and holds the samples
  #that begin within it, sample k beginning at (k - 1) / samp.rate. Both ends
  #were taken as sample numbers and both kept, so a second at 100 Hz was 101
  #samples, the sample between two regions was in both, and a bound between
  #two samples was cut short rather than placed. signif() keeps a product such
  #as 0.07 * 100, which is 7.000000000000001, from being taken as past the
  #sample it names.
  return(c(
    ceiling(signif(t@from * multiplier, 12)) + 1,
    ceiling(signif(t@to * multiplier, 12))
  ))
}

#' Allow subsetting a Wave object with a TimeRegion
#'
#' Applied to a `TaggedWave`, the cut is recorded in the `processing` slot of
#' the result, as described in [tagApply()], with the `from`, `to` and
#' `time.unit` of the region.
#' @param x Wave Object
#' @param i TimeRegion object
#' @return A Wave object containing only the samples within the time region.
setMethod("[", signature(x = "Wave", i = "TimeRegion"), function(x,i){
  return(.tagApplyOne(
    x, .waveInRegion, "[", NULL, FALSE, from=i@from, to=i@to, time.unit=i@unit
  ))
})

#' The part of a wave within a region
#'
#' The region is given by its parts, so that its record names them. Not to be
#' confused with `.cutRegion()` in `tdoa.R`, which places a region's ends by
#' rounding rather than by the samples that begin within it.
#' @param .x A Wave object.
#' @param from,to,time.unit The region.
#' @return The samples of `.x` within the region.
#' @keywords internal
#' @noRd
.waveInRegion <- function(.x, from, to, time.unit) {
  tr <- .timeRegion2samples(
    new("TimeRegion", from=from, to=to, unit=time.unit), .x@samp.rate
  )
  #Clamped either way. Only an infinite end used to be brought back to the
  #length of the wave, so a finite region reaching past it gave a wave padded
  #with NA rather than one ending where the audio does.
  tr[1] <- max(1, tr[1])
  tr[2] <- min(tr[2], length(.x@left))
  if (tr[1] > length(.x@left)) {
    stop("Time region begins after the end of the wave.")
  }
  #A region in time ends before the sample at its end, so one of no length
  #holds no samples.
  samples <- if (tr[2] < tr[1]) integer(0) else tr[1]:tr[2]
  .x@left <- .x@left[samples]
  if (.x@stereo) {
    .x@right <- .x@right[samples]
  }
  return(.x)
}
