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
  #types allowed, and region() checked only its input.unit. A region running
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
#' @param input.unit Unit the from and to values are given in (one of samples,
#'   seconds, minutes, hours)
#' @param from Start time
#' @param to End time
#' @param unit Deprecated, use input.unit instead.
#' @return A TimeRegion object.
#' @export
region <- function(input.unit, from=0, to=Inf, unit) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(unit="input.unit"), "region"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  .validateChoice(
    input.unit, .timeRegionUnits(),
    msg="Unit must be one of samples, seconds, minutes, hours"
  )
  return(new("TimeRegion", from=from, to=to, unit=input.unit))
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
