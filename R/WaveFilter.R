#' WaveFilter object for audio filters
#'
#' A `WaveFilter` object is an object containing information necessary for the
#' `filterWave()` function to apply the filter to a `Wave` or `TaggedWave`
#' object. This is designed to allow a pipe operator (either magrittr or base R)
#' to be used to apply filters to a Wave in a pipeline. If used with a
#' `TaggedWave` object the function adds information to the `processing` slot
#' documenting its action.
#'
#' @slot description Description of the filter.
#' @slot func Name of function.
#' @slot params List of additional parameters to pass to the function.
setClass(
  "WaveFilter",
  slots=list(
    func="function",
    description="character",
    params="list"
  ),
  prototype = list(
    func = function(x) {return(x)},
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
#' @param cl Optional. If a cluster is specified, the filter will be applied in parallel.
#' @export
filterWave <- function(w, filt, cl=NULL) {
  if (inherits(w, c("TaggedWave", "TaggedWaveMC"))) {
    fw <- do.call(match.fun(filt@func), c(list(w), filt@params))
    fw <- addProcess(fw, filt@description)
    return(fw)
  }
  if (inherits(w, c("Wave", "WaveMC"))) {
    return(do.call(match.fun(filt@func), c(list(w), filt@params)))
  }
  if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
    if (is.null(cl)) {
      return(lapply(w, filterWave, filt))
    } else {
      return(parallel::parLapply(cl, w, filterWave, filt))
    }
  }
  stop("Can only filter a Wave or WaveMC object.")
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
#' nwave <- noise("white", duration=44100, samp.rate=44100)
#'
#' fwave <- filterWave(nwave, bandpass(from=1000, to=2000))
#' nwave |> filterWave(bandpass(from=1000, to=2000)) -> fwave
#' }
bandpass <- function( from, to, ...) {
  if (!package.installed("seewave")) {
    stop("seewave package is required for the bandpass WaveFilter.")
  }
  filt <- new("WaveFilter", func=seewave::ffilter, params=list(from=from,to=to,output="Wave",...))
  return(filt)
}
