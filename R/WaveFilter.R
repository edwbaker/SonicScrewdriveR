#' A S4 class to represent a filter for a Wave object
#'
#' A WaveFilter object is an object containing information necessary for the
#' filterw function to apply the filter to a Wave object. This is designed to
#' allow a pipe operator (either magrittr or base R) to be used to apply filters
#' to a Wave in a pipeline.
#'
#' @slot module Module the filter function is found in.
#' @slot func Name of function.
#' @slot allChannels Whether to apply filter to all channels in the wave.
#' @slot params List of additional parameters to pass to the function.
setClass(
  "WaveFilter",
  slots=list(
    func="character",
    allChannels="logical",
    params="list"
  ),
  prototype = list(
    func = NA_character_,
    allChannels= TRUE,
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
filterw <- function(w, filt) {
  if (filt@allChannels==TRUE) {
    return(do.call(allChannels, c(list(w), list(match.fun(filt@func)), filt@params)))
  } else {
    return(do.call(match.fun(filt@func), c(list(w), filt@params)))
  }
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
