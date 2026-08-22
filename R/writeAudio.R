#' Write an audio file
#'
#' This function writes a `Wave` or `WaveMC` object to a file, it is an
#' abstraction function for various specific audio writing functions and is
#' the counterpart of `readAudio()`. WAVE files are written directly, other
#' formats are written by converting a temporary WAVE file using the `av`
#' package. If the `av` package is not available FLAC files can also be written
#' using the `flac` program via the `seewave` package
#' \insertCite{seewave2008}{sonicscrewdriver}.
#'
#' @details
#' The MIME type of the file to write is determined from the file extension,
#' unless it is given in the `mime` argument. Any format supported by the
#' encoders available to the `av` package can be written, commonly used types
#' include:
#' * **audio/x-wav** (`.wav`) - written by `tuneR::writeWave()`.
#' * **audio/flac** (`.flac`)
#' * **audio/mpeg** (`.mp3`)
#' * **audio/mp4** (`.m4a`)
#'
#' Which other formats can be written depends on the encoders available to the
#' `av` package, and some encoders accept only certain sample rates (e.g. Opus
#' requires 48kHz or one of its divisors).
#'
#' Audio written to a lossy format (e.g. MP3) will not be identical to the audio
#' held in the object, and audio written to a format that does not support the
#' bit depth or channel count of the object will be converted by the encoder.
#'
#' @param wave A Wave or WaveMC object to write. The channels of a WaveMC object
#'   must be named, as the name is what records the position of each channel in
#'   the file (see `tuneR::MCnames`). Tagged waves (see `tagWave()`) are written
#'   as the audio they contain, the tags are not saved.
#' @param file File to write.
#' @param mime MIME type of the file to write, or "auto" (the default) to
#'   determine it from the file extension (see Details).
#' @param ... Additional arguments passed to the function performing the write:
#'   `tuneR::writeWave()` for WAVE files, and `av::av_audio_convert()` (e.g.
#'   `bit_rate`) for other formats.
#' @return The path of the file written, invisibly.
#' @references
#'   \insertAllCited{}
#' @export
#' @importFrom tuneR writeWave
#' @importFrom mime guess_type
#' @examples
#' w <- tuneR::sine(440, duration=1000)
#' f <- tempfile(fileext=".wav")
#' writeAudio(w, f)
#' file.remove(f)
#'
#' \dontrun{
#' # Requires the av package
#' writeAudio(w, "tone.mp3")
#' writeAudio(w, "tone.flac")
#' }
#'
writeAudio <- function(wave, file, mime="auto", ...) {
  validateIsWaveLike(wave)
  if (!is.character(file) | length(file) != 1) {
    stop("file must be a single filename")
  }
  #Tags describe the analysis history rather than the audio, and no audio format
  #has anywhere to put them, so they are dropped rather than silently written.
  wave <- untagWave(wave)

  if (mime == "auto") {
    mime <- guess_type(file, unknown=NA_character_)
    if (is.na(mime)) {
      stop("Could not determine the format to write from the file extension, please give a mime type.")
    }
  }
  if (!startsWith(mime, "audio/")) {
    stop(paste("Not an audio mime type:", mime))
  }

  if (mime == "audio/x-wav") {
    writeWave(wave, filename=file, ...)
    return(invisible(file))
  }

  if (package.installed("av", askInstall=FALSE)) {
    return(invisible(.writeAudioAv(wave, file, ...)))
  }
  if (mime == "audio/flac") {
    return(invisible(.writeAudioFlac(wave, file)))
  }
  #Asked for last, so that a missing av package only interrupts the formats that
  #have no other route to disk.
  package.installed("av", askInstall=TRUE)
  return(invisible(.writeAudioAv(wave, file, ...)))
}

#' Write a wave to any format handled by the av package
#'
#' @param wave A Wave or WaveMC object.
#' @param file File to write.
#' @param ... Additional arguments to av::av_audio_convert().
#' @return The path of the file written.
#' @keywords internal
#' @noRd
.writeAudioAv <- function(wave, file, ...) {
  tmp <- tempfile(fileext=".wav")
  on.exit(unlink(tmp), add=TRUE)
  writeWave(wave, filename=tmp)
  #The errors raised by the encoders are hard to act on without knowing that it
  #is the encoder, rather than this package, that has refused the audio.
  tryCatch(
    av::av_audio_convert(tmp, output=file, verbose=FALSE, ...),
    error = function(e) {
      stop(paste0(
        "Could not write ", file, ": ", conditionMessage(e),
        "\nThe encoder for this format may not be available to the av package, ",
        "or may not accept the sample rate or channel count of this wave."
      ), call.=FALSE)
    }
  )
  return(file)
}

#' Write a wave as FLAC using the flac program
#'
#' @param wave A Wave or WaveMC object.
#' @param file File to write.
#' @return The path of the file written.
#' @keywords internal
#' @noRd
.writeAudioFlac <- function(wave, file) {
  tmp <- tempfile(fileext=".wav")
  on.exit(unlink(tmp), add=TRUE)
  writeWave(wave, filename=tmp)
  #wav2flac() writes alongside its input, replacing the extension, so the result
  #has to be moved to where it was asked for.
  out <- paste0(tools::file_path_sans_ext(tmp), ".flac")
  on.exit(unlink(out), add=TRUE)
  seewave::wav2flac(tmp, overwrite=TRUE)
  if (!file.exists(out)) {
    stop("FLAC conversion failed, is the flac program installed?")
  }
  #file.rename() does not work between filesystems, which the temporary
  #directory and the destination may well be on.
  if (!file.rename(out, file)) {
    if (!file.copy(out, file, overwrite=TRUE)) {
      stop(paste("Could not write to", file))
    }
  }
  return(file)
}
