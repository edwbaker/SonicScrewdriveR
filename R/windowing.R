#' Windowing Function for Wave Objects
#'
#' Separates a Wave object into windows of a defined length and runs a function
#' on the window section. Windows may overlap, and the function can make use of
#' 'parallel' package for multi-core processing. It will also show a progress bar
#' if the 'pbapply' package is installed.
#'
#' @details
#' A file is read a window at a time by [readAudio()], so its windows are
#' placed over the audio the file holds, which is what readAudio() reads. A
#' recording cut short, by a flat battery, a full card or a copy that did not
#' finish, declares more audio than it holds, and its windows stop where its
#' audio does: with `complete.windows=TRUE` the last window is the last one its
#' audio fills, and with `complete.windows=FALSE` a final, shorter window holds
#' what is left of it, as for a Wave object holding the same audio. A wave
#' file whose audio size was never written is windowed over the audio
#' readAudio() recovers from it, and a file or Wave object holding no audio
#' gives no windows.
#'
#' The length of a window and the overlap between windows are both given in
#' `time.unit`, which is samples unless it says otherwise. A set of recordings
#' mixes sample rates, and a length in samples is a different length of time in
#' each of them, so `time.unit = "seconds"` gives every recording windows of the
#' same length of time. A length in time is rounded to the nearest whole
#' sample, as [readAudio()] rounds a time it is given. `FUN` is given the start
#' and length of its window in samples, whatever `time.unit` is, as samples are
#' where a window falls.
#'
#' @section Windows in a pipeline:
#' In a [pipeline()], `then=` fills `FUN`: the steps it gives are run on each
#' window, and giving both `then=` and `FUN` is an error. Each window is a
#' tagged wave whose history is that of the wave it was cut from, followed by
#' the cut, so a result from a window traces back to where in the recording it
#' was measured. The results of each step have a row for each window, keyed by
#' `window_start`, `window_end` and `window_length`, in seconds from the start of
#' the recording, counting any cut made before the windowing by [cutws()] or
#' seewave's `cutw()`. A cut made by any other function is not counted, and
#' the windows after it are placed as though it had not been made. A windowing
#' inside another adds columns of its own, numbered by how deep it is:
#' `window_start_2` and so on. A wave shorter than one window gives no windows,
#' which is an error naming the step.
#'
#' @section The order of windowing and filtering:
#' Windowing then filtering does not give the numbers filtering then windowing
#' does. [filterWave()] scales what it gives back to the peak level of the wave
#' it was given. Filtered before windowing, the whole recording is scaled by one
#' factor, so the level of a band can be compared between windows, but the whole
#' recording has to be in memory. Filtered after windowing, each window is
#' scaled by a factor of its own, raising the band in every window to that
#' window's own peak, so the levels of a band compared across windows mislead,
#' but the recording can be read a window at a time. Both orders are sound, and
#' the history of every window records which was used.
#'
#' @param wave A Wave or WaveMC object, or a filename. Using filenames may save
#'   loading an entire large file into memory.
#' @param window.length The length of the analysis window, in `time.unit`.
#' @param window.overlap The overlap between successive windows, in `time.unit`.
#'   A negative value will result in a gap between windows.
#' @param time.unit The unit `window.length` and `window.overlap` are given in:
#'   one of "samples", "seconds", "minutes" or "hours". It comes after `...`, so
#'   that an argument meant for `FUN` is not taken for it, and has to be named in
#'   full.
#' @param bind.wave If TRUE and FUN returns wave objects, then these are
#'   combined into a single object
#' @param FUN FUN to be applied to windows.
#' @param ... Additional parameters to FUN
#' @param complete.windows If TRUE (default) the final window will not be
#'   processed unless it has a length equal to window.length. If FALSE it is
#'   processed holding what is left of the audio, which can be shorter.
#' @param cluster A cluster form the 'parallel' package for multi-core computation.
#' @keywords wave
#' @return A list of the results of FUN for each window, or a single Wave object if bind.wave is TRUE.
#' @export
#' @examples
#' \dontrun{
#' windowing(wave, window.length=1000, FUN=function(w, ...) length(w), window.overlap=0)
#' windowing(wave, 60, FUN=function(w, ...) seewave::H(w), time.unit="seconds")
#' }
windowing <- function(
  wave,
  window.length=1000,
  FUN,
  window.overlap=0,
  bind.wave=FALSE,
  complete.windows=TRUE,
  cluster=NULL,
  ...,
  time.unit="samples"){
  .validateChoice(
    time.unit, .timeRegionUnits(),
    msg="time.unit must be one of samples, seconds, minutes, hours."
  )
  if (typeof(wave) == "character" && .isTriggeredRecording(wave)) {
    # An AudioMoth triggered recording holds neither the length nor the timing
    # of the recording it stands for, so the file on disk cannot be used to
    # schedule the windows: it would give the length with the periods that did
    # not trigger a recording left out, and the last windows of the recording
    # would never be reached. Reading each window from the file separately
    # would also expand the whole recording again for every window, so it is
    # expanded once here instead.
    #
    # A recording whose comment records a trigger can still be one this package
    # cannot expand, a multichannel one for instance, and that keeps the
    # filename it had and is windowed as any other file is. Reading it here
    # instead would hand a WaveMC to a branch that takes only a Wave.
    expanded <- tryCatch(audiomothExpand(wave), unsupportedWav=function(cond) NULL)
    if (!is.null(expanded)) {
      wave <- expanded
    }
  }

  if (typeof(wave) == "character") {
    FUN2 <- function(start, wave, window.length, ...){
      wave <- readAudio(wave, from=start,to=start+window.length-1,time.unit="samples")
      return(FUN(wave, start, window.length, ...))
    }
    n.samples <- .windowingSamples(wave)
    #Read only for a length in time, so that windowing a file in samples reads
    #nothing more than it did.
    samp.rate <- if (time.unit != "samples") {
      readAudio(wave, from=1, to=1, time.unit="samples")@samp.rate
    }
  } else {
    #A file of more than two channels is windowed a window at a time as a WaveMC
    #object, so a WaveMC object is windowed in memory as well.
    validateIsWaveLike(wave)
    n.samples <- length(wave)
    samp.rate <- wave@samp.rate
    FUN2 <- function(start, wave, window.length, ...){
      # Only an incomplete final window reaches past the last sample, and
      # cutws() cuts a point beyond it back to the end of the wave.
      section <- cutws(wave, from=start, to=start+window.length-1)
      return(FUN(section, start, window.length, ...))
    }
  }
  window.length <- .inSamples(window.length, time.unit, samp.rate)
  window.overlap <- .inSamples(window.overlap, time.unit, samp.rate)
  if (window.length < 1) {
    stop("In windowing a window must be at least one sample long.")
  }
  if (bind.wave) {
    if (window.overlap > 0) {
      stop("Cannot bind waves with positive overlap.")
    }
  }

  #An overlap at or above the window length leaves no hop to advance by, so the
  #window count below divided by zero or by a negative number and seq_len()
  #rejected the result with a message about a non-negative integer, naming
  #neither argument.
  if (window.overlap >= window.length) {
    stop("In windowing window.overlap must be less than window.length.")
  }

  n.windows <- ceiling(n.samples / (window.length - window.overlap))

  # Audio that holds no samples has no windows, rather than one starting at a
  # first sample it does not have.
  starts <- (seq_len(n.windows) - 1) * (window.length - window.overlap) + 1
  if (complete.windows) {
    starts <- starts[which(starts <= n.samples - window.length + 1)]
  }

  if (is.null(cluster)){
    if (package.installed("pbapply", askInstall = FALSE)) {
      l <- pbapply::pblapply(starts, FUN2, wave=wave, window.length=window.length, ...)
    } else {
      l <- lapply(starts, FUN2, wave=wave, window.length=window.length, ...)
    }
  } else {
    if (package.installed("pbapply", askInstall = FALSE)) {
      l <- pbapply::pblapply(starts, FUN2, cl=cluster, wave=wave, window.length=window.length, ...)
    } else {
      l <- parallel::parLapply(cluster, starts, FUN2, wave=wave, window.length=window.length, ...)
    }
  }

  #inherits() rather than a test of the first class, which the windows of a
  #tagged wave failed: they are tagged, and came back as a list unbound.
  if (bind.wave && length(l) > 0 && inherits(l[[1]], c("Wave", "WaveMC"))) {
    #tuneR::bind() copies everything it is given, so binding one window at a time
    #copies the whole of the output so far on every iteration. Collecting the parts
    #and binding them in a single call avoids that.
    #
    #The parts are bound untagged. A window of a tagged wave carries the wave's
    #history and a record of its own cut, and bind() keeps the tags of the first
    #part, so the bound wave would hold every sample and a record saying it had
    #been cut to its first window.
    if (window.overlap == 0) {
      l <- do.call(tuneR::bind, untagWave(l))
    } else {
      #With a gap between windows the sections of the wave that no window covers are
      #read back in, so the parts alternate between a window and the gap after it.
      parts <- vector(mode="list", length=2*length(l))
      for (i in 1:length(l)) {
        parts[[2*i-1]] <- l[[i]]
        gap.start <- starts[i] + window.length
        if (i == length(l)) {
          gap.end <- n.samples
        } else {
          gap.end <- min(starts[i+1]-1, n.samples)
        }
        if (gap.end < gap.start) {
          next
        }
        if (inherits(wave, c("Wave", "WaveMC"))) {
          parts[[2*i]] <- cutws(wave, from=gap.start, to=gap.end)
        } else {
          parts[[2*i]] <- readAudio(wave, from=gap.start, to=gap.end, time.unit="samples")
        }
      }
      l <- do.call(tuneR::bind, untagWave(parts[!vapply(parts, is.null, logical(1))]))
    }
    #The bound wave is given the tags of the wave it was cut from, as tagApply()
    #gives them to a wave a function returns untagged.
    if (.mdIsTagged(wave)) {
      l <- tagWave(l, origin=wave@origin)
      l@metadata <- wave@metadata
      l@processing <- wave@processing
    }
  }
  return(l)
}

#' A length or a position in time, in samples
#'
#' Rounded to the nearest whole sample, as readAudio() rounds a time it is given,
#' and kept as it is where it is already in samples. An overlap below zero is a
#' gap, which convert2seconds() refuses as a time, so the size of it is
#' converted and its sign kept.
#' @param x The length.
#' @param time.unit The unit it is in.
#' @param samp.rate The sample rate of the audio it is a length of.
#' @return A number of samples.
#' @keywords internal
#' @noRd
.inSamples <- function(x, time.unit, samp.rate) {
  if (time.unit == "samples") {
    return(x)
  }
  return(sign(x) * round(convert2seconds(abs(x), input.unit=time.unit) * samp.rate))
}

#' How many samples of a file windowing() places its windows over
#'
#' Each window is read by readAudio(), which reads the audio the file holds and
#' refuses a start beyond it, so the windows are placed over that audio rather
#' than the length the header declares. FFmpeg reports the length declared,
#' which for a recording cut short is more than the file holds. A wave file of
#' PCM or floating-point audio is counted as readAudio() counts it, from its
#' chunks, and one whose audio size was never written from the copy
#' readAudio() reads in its place: making that copy once more costs little
#' beside reading the windows, as each of them is read from a copy of its own.
#' Anything else is counted from the duration the av package gives, which is
#' rounded to the microsecond, and so is rounded to the nearest sample: a FLAC
#' file of 3000 samples at 44.1 kHz would otherwise be 2999.99 samples long,
#' too short for a window of 3000, and the gap after the last window is read up
#' to this count, where readAudio() refuses a fraction of a sample.
#'
#' @param file Path to an audio file
#' @return The number of sample frames the file holds
#' @keywords internal
#' @noRd
.windowingSamples <- function(file) {
  # Only a file on disk is counted from its chunks. The av package reads a URL
  # as readily as a path, and says itself what is wrong with something it
  # cannot open, which opening it here first would say instead, and less
  # clearly.
  if (isTRUE(file.info(file)$isdir == FALSE)) {
    repaired <- .wavRepaired(file)
    if (!is.null(repaired)) {
      on.exit(unlink(repaired))
    }
    held <- .wavFramesHeld(if (is.null(repaired)) file else repaired)
    if (!is.null(held)) {
      return(held$frames)
    }
  }
  info <- av::av_media_info(file)
  return(round(info$duration * info$audio[,"sample_rate"]))
}

