#' A S4 class to represent a filter for a Wave object
#'
#' A WaveFilter object is an object containing information necessary for the
#' filterw function to apply the filter to a Wave object. This is designed to
#' allow a pipe operator (either magrittr or base R) to be used to apply filters
#' to a Wave in a pipeline.
#'
#' @slot module Module the filter function is found in.
#' @slot func Name of function.
#' @slot params List of additional parameters to pass to the function.
setClass(
  "WaveFilter",
  slots=list(
    func="character",
    description="character",
    params="list"
  ),
  prototype = list(
    func = NA_character_,
    description=NA_character_,
    params = list()
  )
)

#' Apply a WaveFilter object to a Wave object
#'
#' A WaveFilter object is an object containing information necessary for the
#' filterw function to apply the filter to a Wave object. This is designed to
#' allow a pipe operator (either magrittr or base R) to be used to apply filters
#' to a Wave in a pipeline.
#'
#' Supported filters include those from the seewave package.
#'
#' @param w A Wave object.
#' @param filt Wave object with the selected filter applied.
#' @export
filterWave <- function(w, filt) {
  if(filt@func == "") {
    message("No function supplied to filterWave, will do nothing.")
    filt@func <- "doNowt"
  }
  if (inherits(w, c("TaggedWave", "TaggedWaveMC"))) {
    fw <- do.call(match.fun(filt@func), c(list(w), filt@params))
    fw <- addProcess(fw, filt@description)
    return(fw)
  }
  if (inherits(w, c("Wave", "WaveMC"))) {
    return(do.call(match.fun(filt@func), c(list(w), filt@params)))
  }
  if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
    return(lapply(w, filterWave, filt))
  }
  stop("w must be a Wave or WaveMC object")
}

#' Simple bandpass filter
#'
#' Creates a band pass WaveFilter between values specified to a Wave object.
#'
#' This is a simple wrapper function to the seewave ffilter function allowing its
#' use with filterw and pipes.
#'
#' @param from Bottom of bandpass frequency (Hz).
#' @param to Top of bandpass frequency (Hz).
#' @param ... Further arguments to pass to ffilter.
#' @return A WaveFilter object.
#' @export
#' @examples
#' \dontrun{
#' nwave <- noise("white")
#' fwave <- filterw(nwave, bandpass(from=1000, to=2000))
#' nwave |> filterw(bandpass(from=1000, to=2000)) -> fwave
#' }
bandpass <- function( from, to, ...) {
  filt <- new("WaveFilter", func="ffilter", allChannels=TRUE, params=list(from=from,to=to,output="Wave",...))
  return(filt)
}

#' Do nothing
#'
#' This function does nothing to the Wave (or any object).
#' @param x A thing.
#' @return The same thing.
#' @keywords internal
#' @export
doNowt <- function(x) {return(x)}
