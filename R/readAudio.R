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
#' Which reader a file is given to is decided by the first bytes of the file,
#' not by its extension, as a name is only a convention and the two disagree
#' often enough in a real collection to matter: a wave file called .mp3 read as
#' an MP3 comes back as a shorter recording at the wrong sample rate, without
#' complaint. The name is used only where the bytes identify nothing. Passing
#' `mime` says what the file is and is taken as given, whatever it holds.
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
#' A recorder that loses power part-way through a WAV file can stop before
#' writing the size of the audio, leaving a file that declares an empty
#' recording while still holding the audio it was recording at the time. The
#' audio of such a file is read to the end of the file, with a warning, rather
#' than returned as nothing. A file that really holds no audio is read as empty.
#'
#' A recording too long for the 4 GiB a RIFF header can describe is written in
#' the RF64 or BW64 form, which keeps its sizes in a ds64 chunk.
#' `tuneR::readWave()` does not read these forms, so they are read through the
#' av package, by those sizes. One whose sizes were never written is recovered
#' as above. One whose ds64 chunk disagrees with the rest of the file, or
#' miscounts its sample frames, is still read, with a warning saying what is
#' wrong with it.
#'
#' Samples that are not finite numbers, which a floating-point file can hold,
#' are read as zero with a warning, as they would otherwise spread through every
#' measurement made from the wave.
#'
#' ## Reading part of a recording
#' `from` and `to` give the part of the recording to return. An end point
#' beyond the end of the recording reads to the end of it, as the default end
#' of `Inf` does. A start point at or beyond the end is an error, of class
#' `startPastEnd`, saying how long the recording is, whatever the format of the
#' file and whatever the unit of the start: the part asked for holds no audio
#' at all, and a start past the end is most often one given in the wrong unit,
#' which an empty wave would carry into everything measured from it without
#' saying so. A start of zero is the start of any recording, so a file that
#' holds no audio is read from there as empty.
#'
#' An end point that leaves no audio after the start point is an error of
#' class `endBeforeStart`, for the same reason and in every format and unit
#' alike: a start and an end swapped, or computed from different things, ask
#' for nothing that can be measured. In samples the end point is the last
#' sample returned, so an end at the start returns that one sample, and an end
#' of zero is before the first. In a unit of time the two points are placed at
#' the boundaries between samples nearest to them, so an end at the start holds
#' no audio, nor does a part so short that both of its points are nearest the
#' same boundary. Every format is cut at those same samples, so a part of a
#' recording is the same part however the file holds it, and two parts that
#' meet at a time meet without a sample lost or repeated between them.
#'
#' ## A header that disagrees with the recorder's own metadata
#' A recorder can write a sample rate into its metadata that disagrees with the
#' one in the format chunk of the header, and the header can be the one that is
#' wrong. The wave is returned at the rate the header gives, as that is the rate
#' the samples were read at, and the disagreement is reported with a warning
#' naming both rates: nothing in the file says which of them is right, and a
#' recording read at a tenth of the rate it was made at reads as ten times its
#' length, with every frequency measured from it wrong by the same factor and
#' nothing in the wave to show it.
#'
#' The rates compared against the header are the `Samplerate` of the GUANO
#' metadata and the `F=` rate of each generation of a bext coding history. A
#' time-expanded recording is stored at a lower rate than it was made at, which
#' is no fault: GUANO records the factor in its `TE` field, so a rate that meets
#' the header rate multiplied by that factor is agreement. Of a coding history
#' holding several generations, any one of them meeting the header is enough,
#' as the others describe the recording as it was before it was last written.
#'
#' ## Handling these warnings
#' Each warning above carries a class of its own, so that one of them can be
#' handled without silencing the rest: `sampleRateMismatch`,
#' `unwrittenAudioSize` and `nonFiniteSamples`; `badDs64` and
#' `frameCountMismatch` for an RF64 or BW64 file whose ds64 chunk cannot be
#' believed or miscounts its sample frames; and `truncatedAudio` from
#' [audiomothExpand()]. All of them are also classed `audioFileWarning`, for
#' handling every warning about the file at once, and `warning`, so
#' `suppressWarnings()` and an ordinary warning handler treat them as the
#' warnings they are.
#'
#' ```r
#' withCallingHandlers(
#'   readAudio(file),
#'   sampleRateMismatch = function(cond) invokeRestart("muffleWarning")
#' )
#' ```
#'
#' @param file File to read
#' @param mime MIME type of file to read, or "auto" to read the type from the
#'   bytes of the file rather than from its name. Supported types are
#'   "audio/x-wav" and "audio/mpeg" (MP3); any other type is read by the av
#'   package.
#' @param from Start point in file to return, which must fall within the
#'   recording
#' @param to End point in file to return, which must leave some audio after
#'   the start point, read as the end of the recording where it is beyond it
#' @param input.unit One of "samples", "seconds", "minutes", "hours". Default is "seconds".
#' @param units Deprecated, use input.unit instead.
#' @return A Wave object, or a WaveMC object for a file with more than two
#'   channels.
#' @export
#' @importFrom tuneR readMP3 readWave stereo Wave
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
    mime <- .audioMime(file)
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
      .wavRateWarning(wave, file)
      return(.cutAudio(wave, from, to, input.unit, file))
    }
  }

  repaired <- NULL
  if (mime == "audio/x-wav") {
    # A file whose data chunk size was never written declares that it holds no
    # audio, so the size is filled in on a copy before the file is read. The
    # file itself is left as it is.
    repaired <- .wavRepaired(file)
    if (!is.null(repaired)) {
      on.exit(unlink(repaired), add=TRUE)
    }
    # readWave() cuts the file as it reads it, and returns an empty wave for a
    # start at the end of the audio, or an end just before the start, where it
    # refuses others like them, so the part asked for is held against the
    # audio the file holds before it is asked. It is then asked in samples, at
    # the samples every other reader cuts at, as it places a time half way
    # between two samples a sample away from where they do. A file whose audio
    # cannot be counted from its chunks is asked as it always was.
    start <- fromS
    end <- to
    unit <- input.unit
    if (from != 0 || to != Inf) {
      held <- .wavFramesHeld(if (is.null(repaired)) file else repaired)
      if (!is.null(held)) {
        .startWithin(file, from, input.unit, held$frames, held$rate)
        .endAfterStart(file, from, to, input.unit, held$rate)
        part <- .samplesAsked(from, to, input.unit, held$rate)
        start <- part[1]
        end <- part[2]
        unit <- "samples"
      }
    }
    # Only the reading is guarded: a file readWave() cannot make sense of is
    # read by the av package below instead. What is said about a file that was
    # read is raised, rather than being caught here and the file quietly read a
    # second time by other means. Under options(warn=2) these warnings are
    # errors, and an error that is the warning the caller asked for must reach
    # them.
    wave <- tryCatch(
      readWave(
        if (is.null(repaired)) file else repaired,
        from=start, to=end, units=unit
      ),
      error=function(cond) NULL
    )
    if (!is.null(wave)) {
      if (!is.null(repaired)) {
        .unwrittenWarning(file)
      }
      wave <- .waveFinite(wave, file)
      .wavRateWarning(wave, file)
      return(wave)
    }
  }

  if (mime=="audio/mpeg") {
    # Only the reading is guarded, as it is for a WAV file above: an MP3 file
    # that readMP3() cannot read is read by the av package instead, but what is
    # wrong with the part of it asked for is said of the audio that was read.
    wave <- tryCatch(readMP3(file), error=function(cond) NULL)
    if (!is.null(wave)) {
      return(.cutAudio(wave, from, to, input.unit, file))
    }
  }

  #Check if av package available
  if (package.installed("av", askInstall=TRUE)) {
    # tuneR reads only the RIFF form of a wave file, so a file in the RF64 or
    # BW64 form is read here, by the sizes its ds64 chunk gives. What is wrong
    # with them is said before the audio is read. A copy whose audio size has
    # been filled in is read in place of a file that never had it written, as
    # the av package takes that size at its word and reads such a recording as
    # empty.
    .wavDs64Warnings(file)
    source <- if (is.null(repaired)) file else repaired

    #Using av package
    info <- av::av_media_info(source)$audio
    channels <- info[['channels']]
    if (is.null(channels)) {
      stop("Could not determine number of channels.")
    }

    samples <- av::read_audio_bin(source, channels=channels)
    if (!is.null(repaired)) {
      .unwrittenWarning(file)
    }
    samp.rate <- attr(samples, "sample_rate")
    if (is.null(samp.rate)) {
      samp.rate <- info[['sample_rate']]
    }
    #read_audio_bin() attaches sample_rate and channels to the vector it returns,
    #and they would be carried into the slots of the Wave object if left on it.
    samples <- as.vector(samples)
    samples <- .zeroNonFinite(samples, file)

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

    # A wave file that readWave() could not read is read here instead, and its
    # metadata disagrees with its header whichever of the two read it.
    .wavRateWarning(wave, file)

    return(.cutAudio(wave, from, to, input.unit, file))
  }
  stop("File could not be processed")
}

#' Decide what a file is, from the bytes of it rather than its name
#'
#' A name is only a convention. The corpora this was written against hold wave
#' files under a .mp3 extension and MP3 files under a .wav one, and a reader
#' that goes by the name reads one as the other: `tuneR::readMP3()` does not
#' refuse the PCM of a wave file, it finds a frame header somewhere in the
#' audio and decodes from there, giving back a shorter recording at the wrong
#' sample rate and raising nothing at all.
#'
#' The first bytes of a file say which container it is, so they are read first
#' and the name is used only where they say nothing: a few bytes of raw samples
#' or a format the package has no magic for, where the name is all there is to
#' go on. Bytes that identify any other container, one the av package reads or
#' one this package cannot read at all, give a type that matches neither of the
#' two readers chosen by type, so the file is read as any other such file is.
#'
#' @param file Path to an audio file
#' @return A MIME type
#' @keywords internal
#' @noRd
.audioMime <- function(file) {
  # Only the container is wanted, and .mdContainer() reads it from the first
  # twelve bytes, so nothing more than that is read to find out what the file
  # is. A file that cannot be opened is not reported here: the warning the
  # connection raises on the way to its error is of no use to anyone who is
  # about to be told by the reader that the file cannot be read.
  container <- suppressWarnings(tryCatch(
    .mdContainer(.mdFileHead(file, 12L)), error=function(cond) NA_character_
  ))
  # Bytes that identify nothing, or that could not be read at all, leave the
  # name as the only thing to go on, as it was before any of them were read.
  if (length(container) != 1L || is.na(container)) {
    return(guess_type(file))
  }
  # A wave file in the RF64 or BW64 form is one tuneR::readWave() cannot read,
  # but it goes the way of any other wave file all the same, so that one whose
  # sizes were never written is repaired before the av package is given it.
  if (.mdWaveContainer(container)) {
    return("audio/x-wav")
  }
  if (container == "MP3") {
    return("audio/mpeg")
  }
  # The bytes have already contradicted the name, so the name is not asked
  # again: anything else identifiable is handed to the av package, which is
  # given the file rather than a type to read it as.
  return("application/octet-stream")
}

#' Copy a WAV file whose data size was never written, with the size filled in
#'
#' Only the size of the audio and the RIFF size are changed, so that the audio is
#' read by the same code as any other file. An RF64 or BW64 file keeps them, and
#' the number of sample frames, in its ds64 chunk, and its copy keeps that form.
#' The chunks are walked over a connection, so a long recording is not read into
#' memory to find out whether there is anything to repair. A file with nothing to
#' repair gives NULL, and the file itself is never written to.
#'
#' @param file Path to a WAV file
#' @return Path to a temporary file holding the repaired copy, or NULL
#' @keywords internal
#' @noRd
.wavRepaired <- function(file) {
  total <- file.size(file)
  if (is.na(total) || total < 12) {
    return(NULL)
  }
  info <- tryCatch(.wavChunksFile(file), error=function(cond) NULL)
  if (is.null(info)) {
    return(NULL)
  }
  data <- .wavFindChunk(info$chunks, "data")
  # Asked before the format is read, so that the ordinary file, whose audio is
  # as long as it says it is, costs nothing but the walk.
  if (is.null(data) || data$size != 0) {
    return(NULL)
  }
  fmt <- tryCatch(.wavChunkFromFile(file, "fmt "), error=function(cond) NULL)
  fields <- if (is.null(fmt)) NULL else .wavFmtFields(fmt, length(fmt))
  if (is.null(fields)) {
    return(NULL)
  }
  peek <- function(at, n) {
    con <- file(file, "rb")
    on.exit(close(con))
    return(.wavConReader(con)(at, n))
  }
  size <- .wavUnfinalisedSize(data, info$riffSize, fields$frameSize, total, peek)
  if (size == 0) {
    return(NULL)
  }

  out <- tempfile(fileext=".wav")
  if (!file.copy(file, out)) {
    return(NULL)
  }
  con <- file(out, "r+b")
  on.exit(close(con))
  # A connection open for both reading and writing keeps a position for each, so
  # the write position is the one to set.
  for (patch in .wavSizePatches(info$form, info$ds64, data$start, total - 8, size)) {
    seek(con, patch$at - 1, rw="write")
    writeBin(patch$bytes, con)
  }
  return(out)
}

#' How many sample frames of audio a WAV file holds, and at what rate
#'
#' Counted from the chunks of the file rather than from its header, as a file
#' cut short holds less audio than its data chunk declares, and readWave()
#' reads only the audio it holds. Only PCM and floating-point audio give every
#' sample frame the same number of bytes, so a file holding any other format,
#' and one whose chunks cannot be walked, gives NULL. The formats counted are
#' the ones readWave() reads, which takes an unknown format (0), and an
#' extensible one whose format chunk is too short to name what it extends, as
#' PCM: a file it reads but this does not count would be cut without the part
#' asked for being held against it.
#'
#' @param file Path to a WAV file
#' @return A list of the number of sample frames and the sample rate, or NULL
#' @keywords internal
#' @noRd
.wavFramesHeld <- function(file) {
  info <- tryCatch(.wavChunksFile(file), error=function(cond) NULL)
  data <- if (is.null(info)) NULL else .wavFindChunk(info$chunks, "data")
  if (is.null(data)) {
    return(NULL)
  }
  fmt <- tryCatch(.wavChunkFromFile(file, "fmt "), error=function(cond) NULL)
  fields <- if (is.null(fmt)) NULL else .wavFmtFields(fmt, length(fmt))
  if (is.null(fields) || !fields$format %in% c(0, 1, 3, 65534) || fields$frameSize <= 0) {
    return(NULL)
  }
  return(list(frames=data$size %/% fields$frameSize, rate=fields$sampleRate))
}

#' Warn that the size of the audio in a file was never written
#'
#' @param file The file the audio was read from
#' @return NULL
#' @keywords internal
#' @noRd
.unwrittenWarning <- function(file) {
  .audioFileWarning(
    "unwrittenAudioSize",
    "The size of the audio in '", file, "' was never written, so the ",
    "recording has been read to the end of the file."
  )
  invisible(NULL)
}

#' Say what is wrong with the ds64 chunk of a file whose audio is read anyway
#'
#' The av package reads an RF64 or BW64 file by the sizes in its ds64 chunk,
#' whatever the rest of the file says, so a file whose ds64 chunk this package
#' will not believe still gives audio. That audio is the best reading of the
#' file there is, and is returned, but not without saying why it may not be the
#' whole of the recording.
#'
#' @param file Path to a file about to be read through the av package
#' @return NULL
#' @keywords internal
#' @noRd
.wavDs64Warnings <- function(file) {
  # Only a file on disk is looked at. The av package reads a URL as readily as
  # a path, and says itself what is wrong with something it cannot open, which
  # opening it here first would say instead, and less clearly.
  if (!isTRUE(file.info(file)$isdir == FALSE)) {
    return(invisible(NULL))
  }
  if (!.mdContainer(.mdFileHead(file, 12L)) %in% c("RF64", "BW64")) {
    return(invisible(NULL))
  }
  info <- tryCatch(.wavChunksFile(file), unsupportedWav=function(cond) cond)
  if (inherits(info, "badDs64")) {
    .audioFileWarning(
      "badDs64",
      "The ds64 chunk of '", file, "' cannot be believed, so its audio may ",
      "not be read in full. ", conditionMessage(info)
    )
  } else if (!inherits(info, "unsupportedWav") && !info$ds64$countAgrees) {
    .wavCountWarning(info$ds64, file)
  }
  invisible(NULL)
}

#' Read any sample of a vector that is not a finite number as zero
#'
#' @param samples The samples read from the file
#' @param file The file they were read from
#' @return The samples, with any that are not finite numbers replaced by zero
#' @keywords internal
#' @noRd
.zeroNonFinite <- function(samples, file) {
  bad <- !is.finite(samples)
  if (any(bad)) {
    samples[bad] <- 0
    .nonFiniteWarning(sum(bad), file)
  }
  return(samples)
}

#' Warn that samples were read as zero
#'
#' @param n Number of samples that were not finite numbers
#' @param file The file they were read from
#' @return NULL
#' @keywords internal
#' @noRd
.nonFiniteWarning <- function(n, file) {
  .audioFileWarning(
    "nonFiniteSamples",
    n, " sample", if (n == 1) "" else "s", " of '", file,
    "' ", if (n == 1) "is not a finite number" else "are not finite numbers",
    " and ", if (n == 1) "has" else "have", " been read as zero."
  )
  invisible(NULL)
}

#' Read any sample that is not a finite number as zero
#'
#' A floating-point file can hold values that are not numbers or are infinite,
#' which would otherwise spread through every measurement made from the wave.
#'
#' @param wave A Wave or WaveMC object
#' @param file The file it was read from
#' @return The object, with any such sample replaced by zero
#' @keywords internal
#' @noRd
.waveFinite <- function(wave, file) {
  n <- 0
  if (inherits(wave, "WaveMC")) {
    bad <- !is.finite(wave@.Data)
    if (any(bad)) {
      n <- sum(bad)
      wave@.Data[bad] <- 0
    }
  } else {
    bad <- !is.finite(wave@left)
    if (any(bad)) {
      n <- n + sum(bad)
      wave@left[bad] <- 0
    }
    # A mono wave holds no right channel, so there is nothing to check there.
    bad <- !is.finite(wave@right)
    if (any(bad)) {
      n <- n + sum(bad)
      wave@right[bad] <- 0
    }
  }
  if (n > 0) {
    .nonFiniteWarning(n, file)
  }
  return(wave)
}

#' The first and last sample of the part of a recording asked for
#'
#' Every reader cuts the recording at the samples given here, so that a part
#' of a recording is the same part whatever the format of the file holding it.
#' A time is placed at the boundary between two samples nearest to it, the
#' start at the sample after that boundary and the end at the sample before it,
#' which is where `seewave::cutw()` places them. The end of one part and the
#' start of the next, asked for at the same time, then meet without a sample
#' lost or repeated between them. `tuneR::readWave()` places a start at
#' `round(t * rate + 1)` instead, which differs where a time falls half way
#' between two samples, as R rounds a half to the even number, so it is given
#' these samples rather than the time.
#'
#' A start of zero is the first sample, and an end of `Inf` the last, in any
#' unit. Neither is converted, so that reading a whole file asks nothing of the
#' unit it was given in.
#'
#' @param from Start point as given to readAudio()
#' @param to End point as given to readAudio()
#' @param input.unit The unit `from` and `to` are given in
#' @param samp.rate The sample rate of the recording
#' @return The first and last sample asked for, the last of them `Inf` for an
#'   end of `Inf`
#' @keywords internal
#' @noRd
.samplesAsked <- function(from, to, input.unit, samp.rate) {
  start <- if (from == 0) 1 else from
  end <- to
  if (input.unit != "samples") {
    if (from != 0) {
      start <- round(convert2seconds(from, input.unit=input.unit) * samp.rate) + 1
    }
    #is.infinite() alone would also take -Inf, which convert2seconds() rejects as
    #a negative time and which must keep being rejected.
    if (!(is.infinite(to) && to > 0)) {
      end <- round(convert2seconds(to, input.unit=input.unit) * samp.rate)
    }
  }
  return(c(start, end))
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
#' Every reader but `tuneR::readWave()`, which cuts the file as it reads it,
#' has the whole recording in hand before it is cut: an AudioMoth triggered
#' recording because it is expanded first, and MP3 and everything the av
#' package reads because they are decoded whole. All of them are cut here, so
#' that they stop where each other stops and refuse the same parts, and
#' whatever metadata was read from the file is carried onto the section.
#'
#' @param wave A Wave or WaveMC object
#' @param from Start point to return
#' @param to End point to return
#' @param input.unit One of "samples", "seconds", "minutes", "hours"
#' @param file The file the wave was read from
#' @return A Wave or WaveMC object
#' @keywords internal
#' @noRd
.cutAudio <- function(wave, from, to, input.unit, file) {
  .startWithin(file, from, input.unit, length(wave), wave@samp.rate)
  .endAfterStart(file, from, to, input.unit, wave@samp.rate)
  part <- .samplesAsked(from, to, input.unit, wave@samp.rate)
  # Everything the file said about itself is carried over, not just its GUANO
  # metadata: a section of a recording came from the same recording.
  metadata <- .mdStore(wave)
  # The whole of a recording is returned as it was read, so an empty one is
  # read as empty rather than cut. An end past the last sample stops at it.
  # cutws() keeps every channel, as a wave cut by the samples in it.
  if (part[1] > 1 || part[2] < length(wave)) {
    wave <- cutws(wave, from=part[1], to=min(part[2], length(wave)))
  }
  if (!is.null(metadata)) {
    wave <- .mdSetStore(wave, metadata)
  }
  return(wave)
}

#' Refuse a start point at or beyond the end of the recording
#'
#' A start past the end of a recording leaves nothing of it to read. It is
#' most often a start given in the wrong units, or measured against some other
#' recording: sample 48001 asked for in the default unit of seconds is more
#' than thirteen hours in. An empty wave returned for it would carry the
#' mistake into whatever is measured from it without saying so, so it is
#' refused, in words that say how long the recording is and what was asked
#' for, rather than left to whichever function cutting the wave objects first.
#' Those objected in their own words, and differently for samples and for
#' time.
#'
#' A time is placed at the sample the readers start it at, as
#' `.samplesAsked()` places it, so a start is refused only where they would
#' find nothing after it. A start of zero, or of the first sample, is the start
#' of any recording however short, so an empty recording read from there is
#' read as empty.
#'
#' @param file The file being read, to name in the error
#' @param from Start point as given to readAudio()
#' @param input.unit The unit `from` is given in
#' @param samples The number of sample frames the recording holds
#' @param samp.rate The sample rate of the recording
#' @return NULL, called for the error it raises
#' @keywords internal
#' @noRd
.startWithin <- function(file, from, input.unit, samples, samp.rate) {
  # Nothing is converted for a start of zero, so that reading a whole file
  # asks nothing of the unit it was given in.
  if (from == 0) {
    return(invisible(NULL))
  }
  if (.samplesAsked(from, Inf, input.unit, samp.rate)[1] <= max(1, samples)) {
    return(invisible(NULL))
  }
  if (input.unit == "samples") {
    asked <- paste0("sample ", format(from, scientific=FALSE, trim=TRUE))
    held <- paste0("which holds ", .countText(samples, "sample"))
    where <- "beyond"
  } else {
    asked <- .countText(convert2seconds(from, input.unit=input.unit), "second")
    held <- paste0("which is ", .countText(samples / samp.rate, "second"), " long")
    where <- "at or beyond"
  }
  stop(structure(
    class = c("startPastEnd", "error", "condition"),
    list(
      message = paste0(
        "The start point asked for, ", asked, ", is ", where, " the end of '",
        file, "', ", held, "."
      ),
      call = NULL
    )
  ))
}

#' Refuse an end point that leaves no audio after the start point
#'
#' An end before the start asks for no audio at all, and so, in a unit of
#' time, does an end at the start, or one so close after it that the two are
#' nearest the same boundary between samples. Each was refused by whichever
#' function cut the wave first, in its own words and differently for samples
#' and for time, or read as an empty wave by `tuneR::readWave()` where the
#' other readers gave samples or refused it. Like a start past the end, a part
#' holding nothing is most often a mistake, a start and end swapped or taken
#' from different things, and an empty wave would carry it into whatever is
#' measured from it without saying so. It is refused, saying what was asked
#' for.
#'
#' In samples the end point is the last sample returned, so an end at the
#' start gives that one sample, and an end of zero is before the first, which
#' a start of zero stands for.
#'
#' @param file The file being read, to name in the error
#' @param from Start point as given to readAudio()
#' @param to End point as given to readAudio()
#' @param input.unit The unit `from` and `to` are given in
#' @param samp.rate The sample rate of the recording
#' @return NULL, called for the error it raises
#' @keywords internal
#' @noRd
.endAfterStart <- function(file, from, to, input.unit, samp.rate) {
  part <- .samplesAsked(from, to, input.unit, samp.rate)
  if (part[2] >= part[1]) {
    return(invisible(NULL))
  }
  if (input.unit == "samples") {
    start <- if (part[1] == 1) {
      "the first sample"
    } else {
      paste0("the start point, sample ", format(from, scientific=FALSE, trim=TRUE))
    }
    message <- paste0(
      "The end point asked for, sample ", format(to, scientific=FALSE, trim=TRUE),
      ", is before ", start, ", so no audio of '", file, "' was asked for."
    )
  } else {
    start <- convert2seconds(from, input.unit=input.unit)
    end <- convert2seconds(to, input.unit=input.unit)
    if (end < start) {
      message <- paste0(
        "The end point asked for, ", .countText(end, "second"),
        ", is before the start point, ", .countText(start, "second"),
        ", so no audio of '", file, "' was asked for."
      )
    } else {
      message <- paste0(
        "The part asked for, from ", format(start, scientific=FALSE, trim=TRUE),
        " to ", .countText(end, "second"), ", holds no samples of '", file,
        "', which is sampled at ", .wavRateText(samp.rate), " Hz."
      )
    }
  }
  stop(structure(
    class = c("endBeforeStart", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

#' A count followed by its unit, singular where the count is one
#'
#' @param x A number
#' @param unit The unit, in the singular
#' @return The count and its unit as text, the count never in scientific
#'   notation
#' @keywords internal
#' @noRd
.countText <- function(x, unit) {
  text <- format(x, scientific=FALSE, trim=TRUE)
  return(paste0(text, " ", unit, if (text == "1") "" else "s"))
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

#' Sample rate written as text, however large it is
#'
#' as.character() gives a rate of a million or more in scientific notation,
#' which is not how a sample rate is ever written.
#'
#' @param rates One or more sample rates
#' @return The rates as text
#' @keywords internal
#' @noRd
.wavRateText <- function(rates) {
  return(format(rates, scientific=FALSE, trim=TRUE))
}

#' Read a metadata field as a sample rate
#'
#' A field a reader kept as text, because what was written in it is not a
#' number it could hold, is no rate to compare anything against.
#'
#' @param value The field as its reader gave it
#' @return The rate as a number, or NA where the field holds no number
#' @keywords internal
#' @noRd
.wavRateValue <- function(value) {
  if (length(value) != 1L) {
    return(NA_real_)
  }
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || !is.finite(number)) {
    return(NA_real_)
  }
  return(number)
}

#' Sample rates a bext coding history records
#'
#' The coding history of a Broadcast Wave file holds one line per generation
#' the recording has been through, each a run of comma-separated fields of
#' which `F=` is the sample rate. Every generation is read: a file that has
#' been through an editor carries the rate it was recorded at as well as the
#' rate it is stored at, and the one that meets the header is enough to say the
#' file agrees with itself.
#'
#' @param history The coding history, as bext() gave it
#' @return The rates it records, each once, in the order they appear
#' @keywords internal
#' @noRd
.wavCodingRates <- function(history) {
  if (length(history) != 1L || is.na(history) || !nzchar(history)) {
    return(numeric(0))
  }
  # A field begins a line or follows a separator, so that the F of a field
  # named something ending in F, or a word in a free-text field, is not read as
  # the sample rate of the generation.
  found <- regmatches(
    history, gregexpr("(^|[,;[:space:]])F=[0-9]+", history)
  )[[1]]
  if (length(found) == 0L) {
    return(numeric(0))
  }
  rates <- suppressWarnings(as.numeric(sub("^.*F=", "", found)))
  return(unique(rates[is.finite(rates) & rates > 0]))
}

#' What the metadata of a wave file says its sample rate is
#'
#' Only the metadata chunks are read, not the recording, so asking costs no
#' more than the metadata itself however long the recording is. A file carrying
#' no metadata, or one that is not a wave file at all, says nothing and gives
#' no claims.
#'
#' A time-expanded recording is stored at a lower rate than it was recorded at,
#' which is a legitimate difference rather than a fault. GUANO records the
#' factor in its `TE` field, so that factor is carried alongside the rates for
#' the comparison to allow for.
#'
#' @param file Path to the file
#' @return A list of claims, each a source named as it is to be reported, the
#'   rates that source records, and the time expansion the file declares
#' @keywords internal
#' @noRd
.wavRateClaims <- function(file) {
  # Reading the metadata is a cross-check on the audio, not something the
  # caller asked for, so neither a complaint about how a field is written nor a
  # note that it departs from its specification is raised from under a call to
  # read the audio.
  read <- suppressMessages(suppressWarnings(tryCatch(
    list(guano=guano(file), bext=bext(file)),
    error=function(cond) NULL
  )))
  if (is.null(read)) {
    return(list())
  }
  expansion <- .wavRateValue(read$guano[["TE"]])
  # A file that records no expansion, or an expansion that is not a factor at
  # all, is stored at the rate it was recorded at.
  if (!is.finite(expansion) || expansion < 1) {
    expansion <- 1
  }
  claims <- list()
  declared <- .wavRateValue(read$guano[["Samplerate"]])
  if (is.finite(declared) && declared > 0) {
    claims[[length(claims) + 1L]] <- list(
      source="its GUANO metadata", rates=declared, expansion=expansion
    )
  }
  history <- .wavCodingRates(read$bext[["CodingHistory"]])
  if (length(history) > 0L) {
    claims[[length(claims) + 1L]] <- list(
      source="the coding history of its bext chunk", rates=history,
      expansion=expansion
    )
  }
  return(claims)
}

#' Warn where the header of a file disagrees with its own metadata about the
#' sample rate
#'
#' A recorder can write a sample rate into its metadata that disagrees with the
#' one in the format chunk, and the format chunk can be the one that is wrong.
#' Nothing in the file says which of them is right, so the disagreement is
#' reported rather than corrected: a recording read at a tenth of the rate it
#' was made at reads as ten times its length, and every frequency measured from
#' it is wrong by the same factor, with nothing in the wave to show it.
#'
#' @param wave The wave that was read
#' @param file The file it was read from
#' @return NULL, called for the warning it raises
#' @keywords internal
#' @noRd
.wavRateWarning <- function(wave, file) {
  rate <- wave@samp.rate
  if (length(rate) != 1L || !is.finite(rate) || rate <= 0) {
    return(invisible(NULL))
  }
  disagree <- character(0)
  for (claim in .wavRateClaims(file)) {
    # A rate that meets the header, or that meets it once the time expansion
    # the file declares is allowed for, is agreement. Of a coding history that
    # holds several generations, any one of them meeting the header is enough:
    # the others describe the recording as it was before it was last written.
    if (any(claim$rates == rate | claim$rates == rate * claim$expansion)) {
      next
    }
    disagree <- c(disagree, paste0(
      claim$source, " records ",
      paste(.wavRateText(claim$rates), collapse=" and "), " Hz"
    ))
  }
  if (length(disagree) == 0L) {
    return(invisible(NULL))
  }
  .audioFileWarning(
    "sampleRateMismatch",
    "The sample rate in the header of '", file, "' is ", .wavRateText(rate),
    " Hz, but ", paste(disagree, collapse=", and "), ". Which of them is right ",
    "cannot be known from the file, so the recording has been read at the rate ",
    "its header gives: its duration and every frequency measured from it ",
    "follow from that rate."
  )
  return(invisible(NULL))
}
