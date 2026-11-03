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
#' The result is normalised to the peak of the wave it was given, every channel
#' by the same factor so that they keep their balance. A filter returns its
#' output at whatever level it gives it, and can ring past the level it was
#' given: a full scale square wave low passed at 1 kHz would otherwise peak
#' beyond what 16 bit holds, which [tuneR::writeWave()] refuses. Normalised, a
#' filtered wave fits the format it came in. A filter keeping only part of a
#' recording raises what it keeps to the peak the whole recording had, so the
#' level a band had in the input is not kept.
#'
#' Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
#' `processing` slot of the result, as described in [tagApply()]. The function
#' the record is under is `filterWave`, as it is what filters every channel and
#' normalises the result, and the filter is its arguments: first the filter's
#' function, as `filter`, then its settings, so that two filters with the same
#' settings, and two runs of one filter at different frequencies, are told apart
#' by their records. The function is named as it would be called from outside
#' its package, as `"seewave::bwfilter"`, by its name where it was written at
#' the top level, and as `"<anonymous function>"` where it was written inline.
#' The settings are named as the filter's function names them, the arguments it
#' was not given are recorded as its `defaults`, and the version recorded is
#' that of its package. The description of the filter is the process the record
#' is under, where it has one.
#'
#' @param w A Wave object.
#' @param filt Wave object with the selected filter applied.
#' @param cl Optional. If a cluster is specified, the filter will be applied in parallel.
#' @return A filtered Wave or WaveMC object, or a list of such objects if given a list.
#' @export
filterWave <- function(w, filt, cl=NULL) {
  #The filter's function is named here, once, rather than for each wave. A
  #function written at the top level is found under its name in the global
  #environment of the session that wrote it, which the workers of a cluster do
  #not share, so a worker naming it would record it as anonymous and the same
  #filter would be recorded differently in parallel. It is passed on unforced,
  #so that it is not looked for unless a record is made.
  return(.filterWave(w, filt, cl, .functionName(filt@func)))
}

#' Apply a filter to a wave or a list of waves
#' @param w A Wave-like object, or a list of them.
#' @param filt A WaveFilter object.
#' @param cl A cluster, or `NULL`.
#' @param filter_name The name of the filter's function for the record.
#' @return What [filterWave()] returns.
#' @noRd
.filterWave <- function(w, filt, cl, filter_name) {
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
    #
    #They are described as the filter's function names them and with its
    #defaults, and its package's version is recorded, rather than as arguments
    #of the wrapper that hands them on. The function itself is recorded beside
    #them: the settings alone left two filters with the same cut-offs
    #indistinguishable.
    apply_one <- function(...) {
      .tagApplyOne(
        w, .applyFilter(filt), "filterWave", .filterLabel(filt), TRUE, ...,
        .described=filt@func, .recorded=list(filter=filter_name)
      )
    }
    return(do.call(apply_one, filt@params))
  }
  if (all(sapply(w, inherits, what=c("Wave", "WaveMC", "TaggedWave", "TaggedWaveMC")))) {
    if (is.null(cl)) {
      return(lapply(w, .filterWave, filt, NULL, filter_name))
    } else {
      return(parallel::parLapply(cl, w, .filterWave, filt, NULL, filter_name))
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
#' ffilter returns its output at one part in `wl` of the input's level, 1024 by
#' default, unless asked to rescale it. [filterWave()] normalises the result to
#' the peak of the wave it was given, so it comes back at the level of the input
#' whatever window it was filtered with.
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
  filt <- new("WaveFilter", func=.ffilterWave, params=list(from=from,to=to,...))
  return(filt)
}

#' seewave's ffilter() returning a Wave
#'
#' ffilter() returns a matrix of samples unless asked for a Wave, and the filters
#' put what they return back in a wave of the input's format, so a Wave is always
#' asked for. ffilter() takes its short-term Fourier transform scaled by the
#' window length and inverts it unscaled, so what it passes comes back at one
#' part in `wl` of its level: a full scale 16 bit tone filtered to a peak of 32.
#' `.filterAllChannels()` normalises it to the peak of the input.
#'
#' @param wave A Wave object.
#' @param ... Further arguments to ffilter(), other than its output.
#' @return The filtered Wave object.
#' @noRd
.ffilterWave <- function(wave, ...) {
  return(seewave::ffilter(wave, ..., output="Wave"))
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

  samples <- do.call(cbind, lapply(filtered, function(x) x@left))
  #The channels of a WaveMC are named, and writeWave() builds the channel mask
  #of the file from the names. The filtered channels were bound without them,
  #so a filtered recording was written without the speaker positions it had.
  colnames(samples) <- if (inherits(w, "WaveMC")) colnames(w@.Data)
  return(.rebuildWave(.normaliseToPeak(samples, w) + centre, w))
}

#' Filtered samples normalised to the peak of the wave they came from
#'
#' A filter returns its output at whatever level it gives it. ffilter() from
#' seewave returns what it passes at one part in its window length of the level
#' it had, and a filter can ring past the level it was given: a full scale 16 bit
#' square wave low passed at 1 kHz peaks at 38545, which writeWave() refuses. The
#' filtered samples are scaled so that their peak is the peak of the input, every
#' channel by the same factor so that they keep their balance, and so a filtered
#' wave fits the format it came in. Samples that are all zero, from a filter
#' that passed nothing, are left as they are.
#'
#' Integer PCM reaches one step further below its centre than above it, so an
#' input reaching the bottom of its format has a peak one more than a sample can
#' be above the centre. Where that would put the filtered peak beyond the top of
#' the format, the samples are scaled to the top instead.
#'
#' @param samples The filtered excursions from `.waveCentre()`, a matrix with one
#'   column per channel.
#' @param w The wave that was filtered, whose peak and format the samples take.
#' @return The samples, normalised to the peak of `w`.
#' @noRd
.normaliseToPeak <- function(samples, w) {
  peak <- max(abs(samples), na.rm=TRUE)
  target <- max(abs(.channelMatrix(w) - .waveCentre(w)), na.rm=TRUE)
  #Dividing by the peak first leaves the peak at exactly that of the input,
  #where multiplying by target/peak can leave it a rounding error beyond.
  if (peak > 0 && peak != target) {
    samples <- samples / peak * target
  }
  limit <- .waveFullExcursion(w)
  top <- max(samples, na.rm=TRUE)
  if (w@pcm && top > limit) {
    samples <- samples / top * limit
  }
  return(samples)
}
