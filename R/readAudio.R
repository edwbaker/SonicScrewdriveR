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
    wave[which(is.na(wave))] <- 0
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
