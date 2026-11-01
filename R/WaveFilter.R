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
#' The result has the format of the input, with the filtered samples rounded to
#' whole numbers where that is integer PCM. Every channel is filtered, and eight
#' bit PCM is filtered about its midpoint of 128.
#'
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()]. The settings
#' of the filter are its arguments, so two runs of the same filter at different
#' frequencies are told apart by their records. The description of the filter is
#' the process the record is under, where it has one.
#'
#' @param w A Wave object.
#' @param filt Wave object with the selected filter applied.
#' @param cl Optional. If a cluster is specified, the filter will be applied in parallel.
#' @return A filtered Wave or WaveMC object, or a list of such objects if given a list.
#' @export
filterWave <- function(w, filt, cl=NULL) {
  if (inherits(w, c("Wave", "WaveMC"))) {
    #The filters dispatch on the class of the wave and know nothing of a
    #TaggedWave, so .tagApplyOne() is asked for an untagged one and puts the
    #tags back on what the filter returns. Passing the tagged wave straight to a
    #filter gave an error from seewave.
    #
    #The parameters of the filter are passed on rather than read from the filter
    #inside, so that they reach the record as the arguments of the call. Only
    #the name of the filter was recorded, which left two cut-offs of one filter
    #indistinguishable.
    return(do.call(
      .tagApplyOne,
      c(list(w, .applyFilter(filt), "filterWave", .filterLabel(filt), TRUE), filt@params)
    ))
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

#' A function that applies one filter to a wave
#'
#' The wave is named with a dot for the same reason `.tagApplyOne()`'s arguments
#' are: the settings of the filter arrive in `...`, and R would match a short
#' name against the formals before it. seewave calls a sampling frequency `f`.
#' @param filt A WaveFilter object.
#' @return A function of a wave and the filter's settings.
#' @noRd
.applyFilter <- function(filt) {
  function(.wave, ...) {
    return(.filterAllChannels(.wave, filt))
  }
}

#' The process a filter is recorded under
#'
#' A filter needs no description, and the prototype of the class has none, so a
#' record was left with a process of `NA`. The name of the function is used
#' instead, as it is for any other call.
#' @param filt A WaveFilter object.
#' @return The description of the filter, or `NULL` where it has none.
#' @noRd
.filterLabel <- function(filt) {
  if (length(filt@description) != 1 || is.na(filt@description)) {
    return(NULL)
  }
  return(filt@description)
}

#' Simple bandpass filter
#'
#' Creates a band pass WaveFilter between values specified to a Wave object.
#'
#' This is a simple wrapper function to the seewave ffilter function allowing its
#' use with filterw and pipes.
#'
#' The frequencies the filter passes keep the level they had. ffilter returns
#' its output at one part in `wl` of the input's level, 1024 by default, unless
#' asked to rescale it, so the output is scaled back by `wl`.
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
  filt <- new("WaveFilter", func=.ffilterUnityGain, params=list(from=from,to=to,...))
  return(filt)
}

#' seewave's ffilter() with the level of the frequencies it passes kept
#'
#' ffilter() takes its short-term Fourier transform scaled by the window length
#' and inverts it unscaled, so what it passes comes back at one part in `wl` of
#' its level: a full scale 16 bit tone filtered to a peak of 32. Rounded to the
#' format of the input, a recording peaking at -30 dBFS filtered to zeros in
#' 99.98% of its samples, and eight bit audio to zeros throughout. The output is scaled back by the
#' window length, unless ffilter was asked to rescale it to the range of the
#' input itself. A Wave is always asked for, as the level is put back on its
#' samples.
#'
#' @param wave A Wave object.
#' @param ... Further arguments to ffilter(), other than its output.
#' @param wl,rescale As for ffilter(), whose defaults they repeat so that the
#'   output is scaled by the window length it was filtered with.
#' @return The filtered Wave object.
#' @noRd
.ffilterUnityGain <- function(wave, ..., wl=1024, rescale=FALSE) {
  filtered <- seewave::ffilter(wave, ..., wl=wl, rescale=rescale, output="Wave")
  if (!rescale) {
    filtered@left <- filtered@left * wl
  }
  return(filtered)
}

#' Apply a filter to every channel of a wave
#'
#' The filters read the left slot of a Wave, so passing a stereo Wave or a WaveMC
#' straight to one returned only its first channel, silently discarding the rest.
#' Each channel is filtered separately and the results put back together.
#'
#' A filter returns a wave of its own format, which for seewave is 16 bit PCM
#' whatever it was given, and samples that are not whole. The result is put back
#' in the format of the input, rounded where that is integer PCM, by
#' `.rebuildWave()`. A mono wave is filtered here too, so that it is returned in
#' the format it came in rather than in seewave's.
#'
#' Eight bit PCM is unsigned about a midpoint of 128. A filter passing nothing at
#' 0 Hz removes that offset with the rest of the lowest frequencies, which left
#' the filtered samples either side of zero in a format running from 0 to 255.
#' The excursion from the midpoint is filtered instead, and the midpoint put
#' back afterwards.
#'
#' @param w A Wave or WaveMC object.
#' @param filt A WaveFilter object.
#' @return A wave of the same class, format and channel count as the input.
#' @noRd
.filterAllChannels <- function(w, filt) {
  centre <- .waveCentre(w)
  filtered <- allChannels(
    w,
    function(channel) {
      channel@left <- channel@left - centre
      do.call(match.fun(filt@func), c(list(channel), filt@params))
    },
    channel.param = NULL
  )
  #allChannels() wraps a result that is not a list in one.
  filtered <- lapply(filtered, function(x) if (is.list(x)) x[[1]] else x)

  return(.rebuildWave(do.call(cbind, lapply(filtered, function(x) x@left)) + centre, w))
}
