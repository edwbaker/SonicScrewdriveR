#' An S4 class to represent a filter for a Wave object
#'
#' A WaveFilter object is an object containing information necessary for the
#' filterw function to apply the filter to a Wave object. This is designed to
#' allow a pipe operator (either magrittr or base R) to be used to apply filters
#' to a Wave in a pipeline.
#'
#' @slot module Module the filter function is found in
#' @slot func Name of function
#' @slot params List of additional parameters to pass to the function
setClass(
  "WaveFilter",
  slots=list(
    module="character",
    func="character",
    params="list"
  ),
  prototype = list(
    module = NA_character_,
    func = NA_character_,
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
#' @param w A Wave object
#' @param filt Wave object with the selected filter applied
#' @export
filterw <- function(w, filt) {
  if (filt@module == "seewave") {
    filt@params <- c(filt@params, "output"="Wave")
    return(do.call(match.fun(filt@func), c(w, filt@params)))
  }
}

bandpass <- function(...) {
  filt <- new("WaveFilter", module="seewave", func="ffilter", params=list(...))
  return(filt)
}
