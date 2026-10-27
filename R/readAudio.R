#' Read an audio file
#'
#' This file is used to read an audio file and return a Wave object, it is an abstraction
#' function for various specific audio reading functions. If no existing method can be identified
#' an attempt is made to use the av package to read the audio.
#'
#' A file named following the AudioMoth convention for triggered recordings,
#' ending in T.WAV, is expanded by [audiomothExpand()] as it is read, so that
#' the periods that did not trigger a recording are returned as silence and the
#' timing of the triggered recordings is preserved.
#'
#' A recorder that loses power part-way through a WAV file can leave the size of
#' the audio unwritten, so that the file declares an empty recording while
#' holding the audio it was recording at the time. The audio of such a file is
#' read to the end of the file, with a warning, rather than returned as nothing.
#'
#' Samples that are not finite numbers, which floating-point files can hold, are
#' read as zero with a warning, as they would otherwise spread through any
#' measurement made from the wave.
#'
#' @param file File to read
#' @param mime MIME type of file to read, or "auto". Supported types are "audio/x-wav" and "audio/mpeg" (MP3)
#' @param from Start point in file to return
#' @param to End point in file to return
#' @param input.unit The unit the from and to values are given in, one of
#'   "samples", "seconds", "minutes", "hours". Default is "seconds".
#' @param units Deprecated, use input.unit instead.
#' @return A Wave object
#' @export
#' @importFrom tuneR readMP3 readWave stereo Wave
#' @importFrom seewave cutw
#' @importFrom tools file_ext
#' @importFrom mime guess_type
readAudio <- function(file, mime="auto", from=0, to=Inf, input.unit="seconds", units) {
  if (!missing(units)) {
    .deprecatedArg("units", "input.unit", "readAudio")
    input.unit <- units
  }
  input.unit <- .validateUnit(input.unit, .wavePositionUnits(), "readAudio")
  if (mime == "auto") {
    mime <- guess_type(file)
  }
  if (input.unit=="samples" & from == 0) {
    fromS <- 1
  } else{
    fromS <- from
  }

  if (mime == "audio/x-wav" && .isTWavName(file)) {
    # An AudioMoth triggered recording reads as a valid WAV file, so it has to
    # be expanded before readWave() gets to it. Anything that is not really a
    # T.WAV file falls through to be read normally.
    wave <- tryCatch(audiomothExpand(file), error=function(cond) NULL)
    if (!is.null(wave)) {
      return(.cutAudio(wave, from, to, input.unit, fromS))
    }
  }

  if (mime == "audio/x-wav") {
    # A file whose data chunk size was never written declares that it holds no
    # audio, so the size is filled in on a copy before the file is read.
    repaired <- .wavRepaired(file)
    if (!is.null(repaired)) {
      on.exit(unlink(repaired), add=TRUE)
    }
    tryCatch({
      wave <- readWave(
        if (is.null(repaired)) file else repaired,
        from=fromS, to=to, units=input.unit
      )
      if (!is.null(repaired)) {
        warning(
          "The size of the audio in '", file, "' was never written, so the ",
          "recording has been read to the end of the file."
        )
      }
      return(.waveFinite(wave, file))
    },
    error=function(cond){

    })
  }

  if (mime=="audio/mpeg") {
    wave <- NULL
    wave <- tryCatch({
      wave <- readMP3(file)
      if (input.unit=="samples") {
        return(cutws(wave, fromS, to))
      }
      if (from==0 && to==Inf) {
        return(wave)
      }
      from <- convert2seconds(from, input.unit=input.unit)
      to <- convert2seconds(to, input.unit=input.unit)
      return(cutw(wave,from=from, to=to, output="Wave"))
    },
    error=function(cond){
    })
  }

  #Check if av package available
  if (package.installed("av", askInstall=TRUE)) {
    #Using av package
    channels <- av::av_media_info(file)$audio[['channels']]
    if (is.null(channels)) {
      stop("Could not determine number of channels.")
    }
    if (channels > 2) {
      stop("channel count greater than 2 is not supported")
    }

    wave <- av::read_audio_bin(file, channels=channels)
    nonFinite <- !is.finite(wave)
    if (any(nonFinite)) {
      wave[nonFinite] <- 0
      .nonFiniteWarning(sum(nonFinite), file)
    }
    bit <- .bitdepth(wave)

    if (channels == 1) {
      wave <- Wave(left=wave, samp.rate=attr(wave, "sample_rate"), bit=bit)
    }
    if (channels == 2) {
      left <- wave[seq(1, length(wave), by = 2)]
      right <- wave[seq(2, length(wave), by = 2)]
      wave <- Wave(left=left, right=right, samp.rate=attr(wave, "sample_rate"), bit=bit)
    }

    if (input.unit == "samples") {
      return(cutws(wave,from=fromS, to=to))
    } else {
      if (from==0 & to == Inf) {
        return(wave)
      }
      if (channels == 1) {
        return(cutw(wave, from=convert2seconds(from, input.unit), to=convert2seconds(to, input.unit), output="Wave"))
      }
      if (channels == 2) {
        left <- cutw(wave, channel=1, from=convert2seconds(from, input.unit), to=convert2seconds(to, input.unit), output="Wave")
        right <- cutw(wave, channel=2, from=convert2seconds(from, input.unit), to=convert2seconds(to, input.unit), output="Wave")
        return(stereo(left, right))
      }
    }
  }
  stop("File could not be processed")
}

#' Copy a WAV file whose data size was never finalised, with the size filled in
#'
#' Only the size of the audio and the size in the RIFF header are changed, so
#' that the audio is read by the same code as any other file. A file that has
#' nothing to repair gives NULL, and the file itself is never written to.
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
  # The header is all that has to be read to find out whether there is anything
  # to repair, and a file holding more metadata than this before its audio is
  # left to be read as it is.
  bytes <- tryCatch(
    readBin(file, "raw", n=min(total, 65536)),
    error=function(cond) NULL
  )
  if (is.null(bytes)) {
    return(NULL)
  }
  info <- tryCatch(.wavFormat(bytes, total=total), error=function(cond) NULL)
  if (is.null(info) || !isTRUE(info$dataUnfinalised)) {
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
  seek(con, 4L, rw="write")
  writeBin(.uintToRaw(total - 8, 4L), con)
  # The size of a chunk is held in the four bytes before its body.
  seek(con, info$dataStart - 5L, rw="write")
  writeBin(.uintToRaw(info$dataSize, 4L), con)
  return(out)
}

#' Warn that samples were read as zero
#'
#' @param n Number of samples that were not finite numbers
#' @param file The file they were read from
#' @return NULL
#' @keywords internal
#' @noRd
.nonFiniteWarning <- function(n, file) {
  warning(
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

#' Return the requested part of a wave that was read whole
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
    cut <- cutws(wave, from=fromS, to=to)
  } else if (from == 0 && to == Inf) {
    cut <- wave
  } else {
    cut <- cutw(
      wave,
      from = convert2seconds(from, input.unit=input.unit),
      to = convert2seconds(to, input.unit=input.unit),
      output = "Wave"
    )
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
