#' Read an audio file
#'
#' This file is used to read an audio file and return a Wave object, it is an abstraction
#' function for various specific audio reading functions. If no existing method can be identified
#' an attempt is made to use the av package to read the audio.
#'
#' @details
#' Mono and stereo files are returned as a `tuneR::Wave` object, and files with
#' more than two channels as a `tuneR::WaveMC` object holding one column per
#' channel, in the order the file holds them. `writeAudio()` writes either back
#' out, and functions that take multichannel audio, such as `tdoa()`, take the
#' result directly.
#'
#' WAVE files are read by `tuneR::readWave()` whatever their channel count, so a
#' multichannel WAVE keeps the channel names the file itself records. Everything
#' read through the av package is named by channel position instead (see
#' `tuneR::MCnames`), because the av package reports a channel layout that does
#' not identify the channels of the file.
#'
#' Files read through the av package (which is everything that is not WAVE or
#' MP3, e.g. FLAC) are returned at the bit depth the file was stored at, so that
#' the same audio compares equal however it was read. The av package does not
#' report a bit depth of 24, and returns 24bit audio as 32bit, so the samples of
#' a 24bit file are 256 times those `tuneR::readWave()` would give for the same
#' audio stored as WAVE. Audio stored as floating point is returned as 32bit.
#'
#' A recording made on a trigger is expanded by [audiomothExpand()] as it is
#' read, so that the periods that did not trigger a recording are returned as
#' silence and the timing of the triggered recordings is preserved. Such a
#' recording is recognised by the trigger AudioMoth records in the comment it
#' writes into the header, rather than by the T before its extension: a name is
#' only a convention, which an ordinary recording can meet and a triggered one
#' can be renamed out of. A file whose comment has been lost can still be
#' expanded by calling [audiomothExpand()] on it.
#'
#' @param file File to read
#' @param mime MIME type of file to read, or "auto". Supported types are "audio/x-wav" and "audio/mpeg" (MP3)
#' @param from Start point in file to return
#' @param to End point in file to return
#' @param input.unit One of "samples", "seconds", "minutes", "hours". Default is "seconds".
#' @param units Deprecated, use input.unit instead.
#' @return A Wave object, or a WaveMC object for a file with more than two
#'   channels.
#' @export
#' @importFrom tuneR readMP3 readWave stereo Wave
#' @importFrom seewave cutw
#' @importFrom tools file_ext
#' @importFrom mime guess_type
readAudio <- function(file, mime="auto", from=0, to=Inf, input.unit="seconds",
                      units) {
  redone <- .withRenamedArgs(
    sys.call(), parent.frame(), c(units="input.unit"), "readAudio"
  )
  if (!is.null(redone)) {
    return(redone[[1]])
  }
  if (mime == "auto") {
    mime <- guess_type(file)
  }
  if (input.unit=="samples" & from == 0) {
    fromS <- 1
  } else{
    fromS <- from
  }

  # Asked before the type is weighed, as the type is guessed from the name and
  # a triggered recording can be called anything at all. One that is not a
  # wave file this package can expand says so and is read as it would be
  # otherwise, so there is nothing to lose by asking.
  if (.isTriggeredRecording(file)) {
    # An AudioMoth triggered recording reads as a valid WAV file, so it has to
    # be expanded before readWave() gets to it. A file that is not one of those
    # at all falls through to be read normally, but a failure to expand a
    # recording that is one is raised: reading it by other means would return
    # the periods that did not trigger a recording still missing, and so the
    # events in it at the wrong times, without saying so.
    wave <- tryCatch(
      audiomothExpand(file),
      unsupportedWav = function(cond) NULL
    )
    if (!is.null(wave)) {
      return(.cutAudio(wave, from, to, input.unit, fromS))
    }
  }

  if (mime == "audio/x-wav") {
    tryCatch({
      wave <- readWave(file, from=fromS, to=to, units=input.unit)
      return(wave)
    },
    error=function(cond){

    })
  }

  if (mime=="audio/mpeg") {
    wave <- NULL
    wave <- tryCatch({
      wave <- readMP3(file)
      if (input.unit=="samples") {
        return(cutws(wave, fromS, min(to, length(wave))))
      }
      if (from==0 && to==Inf) {
        return(wave)
      }
      return(.cutSeconds(wave, from, to, input.unit))
    },
    error=function(cond){
    })
  }

  #Check if av package available
  if (package.installed("av", askInstall=TRUE)) {
    #Using av package
    info <- av::av_media_info(file)$audio
    channels <- info[['channels']]
    if (is.null(channels)) {
      stop("Could not determine number of channels.")
    }

    samples <- av::read_audio_bin(file, channels=channels)
    samp.rate <- attr(samples, "sample_rate")
    if (is.null(samp.rate)) {
      samp.rate <- info[['sample_rate']]
    }
    #read_audio_bin() attaches sample_rate and channels to the vector it returns,
    #and they would be carried into the slots of the Wave object if left on it.
    samples <- as.vector(samples)
    samples[which(is.na(samples))] <- 0

    #read_audio_bin() fills the 32bit range whatever the file was stored at, so
    #the samples are scaled back down to the bit depth of the source. Without
    #this the same audio read as WAVE and as FLAC does not compare equal.
    bit <- .avBitdepth(info[['sample_fmt']], samples)
    samples <- samples / 2^(32 - bit)
    if (bit == 8) {
      #8bit PCM is unsigned, which is how readWave() returns it.
      samples <- samples + 128
    }

    if (channels == 1) {
      wave <- Wave(left=samples, samp.rate=samp.rate, bit=bit)
    }
    if (channels == 2) {
      left <- samples[seq(1, length(samples), by = 2)]
      right <- samples[seq(2, length(samples), by = 2)]
      wave <- Wave(left=left, right=right, samp.rate=samp.rate, bit=bit)
    }
    if (channels > 2) {
      wave <- .avWaveMC(samples, channels, samp.rate, bit)
    }

    if (input.unit == "samples") {
      return(cutws(wave,from=fromS, to=min(to, length(wave))))
    } else {
      if (from==0 & to == Inf) {
        return(wave)
      }
      return(.cutSeconds(wave, from, to, input.unit))
    }
  }
  stop("File could not be processed")
}

#' Cut a wave for readAudio(), in time units
#'
#' Shared by the MP3 and av paths of readAudio() so that both cut at the same
#' point and both keep every channel. `seewave::cutw()` returns one channel per
#' call, so a stereo wave is cut a channel at a time and recombined, which a
#' single call would silently reduce to mono.
#'
#' @param wave The Wave object to cut.
#' @param from Start point as given to readAudio().
#' @param to End point as given to readAudio().
#' @param input.unit The unit `from` and `to` are given in.
#' @return A Wave object holding the section asked for, with as many channels as
#'   it was given.
#' @keywords internal
#' @noRd
.cutSeconds <- function(wave, from, to, input.unit) {
  from <- convert2seconds(from, input.unit=input.unit)
  to <- .cutEnd(to, input.unit, wave)
  if (inherits(wave, "WaveMC")) {
    #cutw() takes a Wave and returns one channel, so there is nothing it can do
    #with a WaveMC. The time bounds are turned into the sample positions cutw()
    #would have cut at, and the cut made by row, which keeps every channel. The
    #end is held within the wave against the rounding of a time .cutEnd() has
    #already put at its last sample.
    return(cutws(
      wave,
      from=round(from * wave@samp.rate) + 1,
      to=min(length(wave), round(to * wave@samp.rate))
    ))
  }
  if (wave@stereo) {
    left <- cutw(wave, channel=1, from=from, to=to, output="Wave")
    right <- cutw(wave, channel=2, from=from, to=to, output="Wave")
    return(stereo(left, right))
  }
  return(cutw(wave, from=from, to=to, output="Wave"))
}

#' End point of a cut, in seconds, within the audio in hand
#'
#' readAudio()'s `to` defaults to Inf, and `seewave::cutw()` rejects an end
#' point past the end of the recording, so reading from a non-zero start with no
#' end has to be cut to the end of the wave rather than at the Inf it was given.
#' An end point beyond the recording is treated the same way, giving the audio
#' that is there instead of an error.
#'
#' @param to End point as given to readAudio().
#' @param input.unit The unit `to` is given in.
#' @param wave The Wave object being cut.
#' @return The end point in seconds, no later than the end of the wave.
#' @keywords internal
#' @noRd
.cutEnd <- function(to, input.unit, wave) {
  end <- length(wave) / wave@samp.rate
  #is.infinite() alone would also take -Inf, which convert2seconds() rejects as
  #a negative time and which must keep being rejected.
  if (is.infinite(to) && to > 0) {
    return(end)
  }
  return(min(convert2seconds(to, input.unit), end))
}

#' Build a WaveMC from the samples the av package decoded
#'
#' av::read_audio_bin() returns the samples of every channel in one vector,
#' interleaved a frame at a time, which a matrix filled by row turns back into
#' one column per channel.
#'
#' The channels are named because tuneR::writeWave() refuses a WaveMC whose
#' channels are not, so an unnamed one could not be written back out. They are
#' named by their position in the file, in the order tuneR::MCnames lists,
#' rather than from the layout the av package reports: that layout does not
#' identify the channels, giving "4.0" for a file holding FL/FR/BL/BR as
#' readily as for one holding FL/FR/FC/BC. The names are therefore a
#' convention, not a reading of the file, and a file read as WAVE by
#' tuneR::readWave() keeps the channel names the file itself records.
#'
#' @param samples The decoded samples, already scaled to the bit depth of the
#'   source.
#' @param channels The number of channels in the file.
#' @param samp.rate The sample rate of the file.
#' @param bit The bit depth of the file.
#' @return A WaveMC object with one column per channel.
#' @keywords internal
#' @noRd
.avWaveMC <- function(samples, channels, samp.rate, bit) {
  #A truncated final frame would otherwise be recycled into a full one by
  #matrix(), inventing samples in the channels it did not reach.
  frames <- length(samples) %/% channels
  if (frames < 1) {
    stop("Fewer samples than channels were decoded.")
  }
  samples <- samples[seq_len(frames * channels)]
  m <- matrix(samples, ncol=channels, byrow=TRUE)
  colnames(m) <- .mcChannelNames(channels)
  return(tuneR::WaveMC(m, samp.rate=samp.rate, bit=bit))
}

#' Names for the channels of a wave, by their position
#'
#' tuneR::MCnames holds the channels of the WAVE channel mask in the order the
#' format puts them in, so the first n of them name the first n channels of a
#' file. Positions beyond the ones tuneR knows are numbered instead; the audio
#' is then still readable, although tuneR::writeWave() will not write a wave
#' holding channels it has no name for.
#'
#' @param channels The number of channels to name.
#' @return A character vector of channel names.
#' @keywords internal
#' @noRd
.mcChannelNames <- function(channels) {
  known <- tuneR::MCnames$name
  if (channels <= length(known)) {
    return(known[seq_len(channels)])
  }
  return(c(known, paste0("C", seq(length(known) + 1, channels))))
}

#' Return the requested part of a wave that was read whole
#'
#' An AudioMoth triggered recording is expanded before it is cut, so unlike the
#' other paths it has the whole recording in hand already. It is cut through the
#' same helpers so that it stops where they stop, and any GUANO metadata read
#' from the file is carried onto the section.
#'
#' @param wave A Wave object
#' @param from Start point to return
#' @param to End point to return
#' @param input.unit One of "samples", "seconds", "minutes", "hours"
#' @param fromS Start point adjusted for sample numbering
#' @return A Wave object
#' @keywords internal
#' @noRd
.cutAudio <- function(wave, from, to, input.unit, fromS) {
  metadata <- attr(wave, "guano")
  if (input.unit == "samples") {
    # cutws() indexes the samples directly, so an end past the last of them
    # would pad the result with NA rather than stop at the end of the
    # recording.
    cut <- cutws(wave, from=fromS, to=min(to, length(wave)))
  } else if (from == 0 && to == Inf) {
    cut <- wave
  } else {
    cut <- .cutSeconds(wave, from, to, input.unit)
  }
  if (!is.null(metadata)) {
    attr(cut, "guano") <- metadata
  }
  return(cut)
}

.bitdepth <- function(v) {
  m <- ceiling(max(abs(v), na.rm=TRUE))
  if (m <= 128) { return(8) }
  if (m <= 32768) { return(16) }
  if (m <= 8388608) { return(24) }
  if (m <= 2147483648) { return(32) }
  stop("Bit depths above 32bit are not supported.")
}

#' Bit depth of the audio decoded by the av package
#'
#' av::read_audio_bin() returns samples filling the 32bit range whatever the
#' source was stored at, so the bit depth has to come from the sample format
#' the av package reports for the file. Note that the av package does not
#' expose bits_per_raw_sample, and ffmpeg decodes 24bit audio into a 32bit
#' sample format, so a 24bit source is reported here as 32bit. The samples are
#' exact multiples of 256 in that case, so nothing is lost, but the values are
#' 256 times those readWave() would give for the same audio stored as WAVE.
#'
#' @param sample_fmt The sample_fmt given by av::av_media_info().
#' @param samples The decoded samples, used only if sample_fmt is unusable.
#' @return A bit depth, one of 8, 16 or 32.
#' @keywords internal
#' @noRd
.avBitdepth <- function(sample_fmt, samples) {
  if (length(sample_fmt) != 1 || is.na(sample_fmt)) {
    return(.bitdepth(samples))
  }
  #Planar formats hold each channel in its own buffer, but describe the same
  #sample depth as their interleaved counterparts.
  fmt <- sub("p$", "", as.character(sample_fmt))
  bit <- switch(
    fmt,
    "u8" = 8,
    "s16" = 16,
    #Floating point audio has no bit depth of its own, and read_audio_bin() has
    #already made 32bit integers of it.
    "s32" = 32,
    "s64" = 32,
    "flt" = 32,
    "dbl" = 32,
    NULL
  )
  if (is.null(bit)) {
    return(.bitdepth(samples))
  }
  return(bit)
}
