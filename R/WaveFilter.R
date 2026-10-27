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
#' @slot func The function that applies the filter.
#' @slot func_name Name of the function, as `pkg::name` for a function in a
#'   package. This is what is recorded in the `processing` slot, and it lets a
#'   filter be rebuilt from its record. `NA` if the filter has no name.
#' @slot params List of additional parameters to pass to the function.
setClass(
  "WaveFilter",
  slots=list(
    func="function",
    func_name="character",
    description="character",
    params="list"
  ),
  prototype = list(
    func = function(x) {return(x)},
    func_name=NA_character_,
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
#' @return A filtered Wave or WaveMC object, or a list of such objects if given a list.
#' @export
filterWave <- function(w, filt, cl=NULL) {
  if (inherits(w, c("Wave", "WaveMC"))) {
    label <- if (is.na(filt@description)) NULL else filt@description
    func_name <- if (is.na(filt@func_name)) "<anonymous function>" else filt@func_name
    # Functions from other packages, such as seewave::ffilter, are given an
    # untagged wave by .tagApplyOne(), which puts the tags back on the result.
    return(do.call(.tagApplyOne, c(list(w, filt@func, func_name, label, NA), filt@params)))
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
  filt <- new("WaveFilter", func=seewave::ffilter, func_name="seewave::ffilter", description=paste0("bandpass ", from, "-", to, " Hz"), params=list(from=from,to=to,output="Wave",...))
  return(filt)
}

#' Rebuild a WaveFilter from a process record
#'
#' Looks up the function named in a record made by [filterWave()] and builds the
#' `WaveFilter` that made it, so that the same filter can be applied again.
#'
#' @param record One entry of the `processing` slot of a `TaggedWave` or
#'   `TaggedWaveMC`.
#' @return A `WaveFilter` object.
#' @keywords internal
#' @noRd
.filterFromRecord <- function(record) {
  func_name <- record$func
  if (is.null(func_name) || is.na(func_name) || identical(func_name, "<anonymous function>")) {
    stop("The record does not name the function, so the filter cannot be rebuilt.")
  }
  parts <- strsplit(func_name, ":::?")[[1]]
  func <- if (length(parts) == 2) {
    utils::getFromNamespace(parts[2], parts[1])
  } else {
    match.fun(func_name)
  }
  params <- record$params
  if (is.null(params)) {
    params <- list()
  }
  new("WaveFilter", func = func, func_name = func_name, description = record$process, params = params)
}
