#' Calculate what can be measured of a wave and the file it was read from
#'
#' Works out the hash of the file a wave was read from and of the samples it
#' holds, with its length, format and size, and keeps them in the `metadata`
#' slot of the wave as `calculatedMetadata`, beside the `audioMetadata` read
#' from the file. It only calculates: nothing is compared with what the source
#' of a recording says of it.
#'
#' The fields are those audioBlast calculates of the recordings it holds, under
#' the same names without their `calculated_` prefix, with the hash of the
#' samples added:
#'
#' * `hash`: the SHA-256 hash of the bytes of the file, in lower case, as
#'   audioBlast and the `hash` column of a [RecordingSet][recordingSet()] hold
#'   it.
#' * `hash_method`: the digest `hash` is, `"sha256"`.
#' * `wave_hash`: a SHA-256 digest of every sample the wave holds, as
#'   [tagApply()] takes of the waves it records under
#'   `options(sonicscrewdriver.provenance="full")`. Two files holding the same
#'   audio under different tags have the same wave hash and different file
#'   hashes.
#' * `wave_hash_method`: `"full"`, as every sample is read.
#' * `frames`: the number of sample frames the wave holds.
#' * `duration`: `frames` divided by `sample_rate`, in seconds.
#' * `sample_rate`: in Hz.
#' * `channels`: the number of channels.
#' * `bit_depth`: the bits a sample takes in the file, or `NA` for a lossy
#'   format such as MP3, which stores no samples as such.
#' * `bit_rate`: the bits a second of audio takes in the file.
#' * `codec`: the format the audio is in, as FFmpeg names it, e.g.
#'   `"pcm_s16le"` or `"mp3"`.
#' * `bytes`: the size of the file.
#' * `at`: when they were calculated, in UTC.
#' * `by`: the version of sonicscrewdriver that calculated them.
#'
#' The file is the one [readAudio()] read the wave from, which it records on
#' the wave. The file is hashed when this is called, so it should be called
#' before the file is changed or deleted. A wave read from part of a file has
#' the `hash` and `bytes` of the whole file, and the `frames` and `duration` of
#' the part. A wave that was not read from a file has `NA` for `hash`,
#' `hash_method`, `bit_rate`, `codec` and `bytes`, and its own bit depth.
#'
#' For a wave file, `codec`, `bit_depth` and `bit_rate` are read from its format
#' chunk. For any other file they are asked of the av package where it is
#' installed, and are otherwise `NA`, but for the `codec` of an MP3 file.
#'
#' The fields describe the file as it was read. They are carried unchanged onto
#' the waves [tagApply()] makes from the wave, and are not calculated again when
#' a process changes its samples, as the record of each process holds a digest
#' of the wave it made.
#'
#' Hashing every sample takes about a second per minute of stereo 48 kHz audio.
#'
#' @param w A Wave, WaveMC, TaggedWave or TaggedWaveMC object. A Wave or WaveMC
#'   object is tagged first.
#' @return A TaggedWave or TaggedWaveMC object holding the fields in
#'   `w@metadata$calculatedMetadata`.
#' @export
#' @examples
#' f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
#' w <- calculateMetadata(readAudio(f))
#' w@metadata$calculatedMetadata$hash
#' w@metadata$calculatedMetadata$duration
calculateMetadata <- function(w) {
  if (!inherits(w, c("Wave", "WaveMC"))) {
    stop("calculateMetadata() takes a Wave, WaveMC, TaggedWave or TaggedWaveMC object.")
  }
  w <- tagWave(w)
  multi <- methods::is(w, "WaveMC")
  frames <- if (multi) nrow(w@.Data) else length(w@left)
  channels <- if (multi) ncol(w@.Data) else if (w@stereo) 2L else 1L
  w@metadata[["calculatedMetadata"]] <- .calculatedFields(
    w, frames, channels, .waveHash(w, "full")
  )
  return(w)
}

#' The fields calculateMetadata() keeps
#'
#' Given the length and the digest of the samples rather than working them out
#' from the wave, so that a file too long to read whole is given the same
#' fields from its header and a digest taken a block at a time.
#' @param w A tagged wave, which need hold no samples, recording the file it
#'   was read from.
#' @param frames,channels The sample frames and channels of the recording.
#' @param wave_hash The full digest of its samples. It is worked out only once
#'   the file has been found to exist.
#' @return A list.
#' @keywords internal
#' @noRd
.calculatedFields <- function(w, frames, channels, wave_hash) {
  file <- .fileStore(w)
  if (!is.null(file) && !file.exists(file)) {
    stop("The file this wave was read from no longer exists: ", file)
  }
  coding <- if (is.null(file)) {
    list(bit_depth = w@bit, bit_rate = NA_real_, codec = NA_character_)
  } else {
    .fileCoding(file, w)
  }
  return(list(
    # The digest audioBlast and a RecordingSet hold, so that one can be read
    # against the other.
    hash = if (is.null(file)) NA_character_ else .audioblast_sha256(file),
    hash_method = if (is.null(file)) NA_character_ else "sha256",
    wave_hash = wave_hash,
    wave_hash_method = "full",
    frames = frames,
    duration = frames / w@samp.rate,
    sample_rate = w@samp.rate,
    channels = channels,
    bit_depth = coding$bit_depth,
    bit_rate = coding$bit_rate,
    codec = coding$codec,
    bytes = if (is.null(file)) NA_real_ else file.size(file),
    at = structure(Sys.time(), tzone = "UTC"),
    by = paste("sonicscrewdriver", getNamespaceVersion("sonicscrewdriver"))
  ))
}

#' How the audio of a file is coded
#'
#' A wave file holding PCM or floating-point samples says all of this in its
#' format chunk, which is read without the av package. Anything else, a wave
#' file in a compressed format among them, is asked of av, which names it as
#' FFmpeg does, the names audioBlast holds.
#'
#' @param file The file the wave was read from
#' @param w The wave read from it
#' @return A list of the bit depth, the bit rate and the codec
#' @keywords internal
#' @noRd
.fileCoding <- function(file, w) {
  mime <- .audioMime(file)
  if (identical(mime, "audio/x-wav")) {
    fmt <- tryCatch(.wavChunkFromFile(file, "fmt "), error=function(cond) NULL)
    fields <- if (is.null(fmt)) NULL else .wavFmtFields(fmt, length(fmt))
    if (!is.null(fields) && fields$format %in% c(1, 3)) {
      return(list(
        bit_depth = fields$bits,
        bit_rate = fields$sampleRate * fields$bits * fields$channels,
        codec = .wavCodec(fields$format, fields$bits)
      ))
    }
  }
  if (package.installed("av", askInstall=FALSE)) {
    info <- tryCatch(av::av_media_info(file)$audio, error=function(cond) NULL)
    if (!is.null(info) && nrow(info) > 0) {
      codec <- .ffmpegCodec(as.character(info$codec[1]))
      return(list(
        bit_depth = if (.losslessCodec(codec)) w@bit else NA_real_,
        bit_rate = as.numeric(info$bitrate[1]),
        codec = codec
      ))
    }
  }
  return(list(
    bit_depth = NA_real_,
    bit_rate = NA_real_,
    codec = if (identical(mime, "audio/mpeg")) "mp3" else NA_character_
  ))
}

#' The name FFmpeg gives the samples of a wave file
#'
#' @param format The format a format chunk gives: 1 for PCM, 3 for floating
#'   point
#' @param bits The bits a sample takes
#' @return A codec name, such as "pcm_s16le"
#' @keywords internal
#' @noRd
.wavCodec <- function(format, bits) {
  if (format == 3) {
    return(paste0("pcm_f", bits, "le"))
  }
  # Eight-bit PCM is unsigned, and every wider kind signed.
  if (bits == 8) {
    return("pcm_u8")
  }
  return(paste0("pcm_s", bits, "le"))
}

#' The name of a codec, from the name of the decoder av reports
#'
#' av gives the decoder that read a file, which FFmpeg names after the codec
#' with a suffix for how it decodes, or a prefix for the library it uses:
#' "mp3float" for MP3, "aac_fixed" for AAC, "libopus" for Opus. audioBlast holds
#' the codec's own name, as ffprobe gives it.
#'
#' @param decoder A decoder name
#' @return A codec name
#' @keywords internal
#' @noRd
.ffmpegCodec <- function(decoder) {
  return(sub("^lib", "", sub("(_fixed|float)$", "", decoder)))
}

#' Does a codec keep every sample as it was?
#'
#' A lossless codec stores samples of a bit depth, which decoding gives back. A
#' lossy one stores none, so the bit depth of what decoding gives is not one
#' the file has. A-law and mu-law are named as PCM, but each stores eight bits
#' that decode to sixteen.
#'
#' @param codec A codec name, as FFmpeg gives it
#' @return A logical value
#' @keywords internal
#' @noRd
.losslessCodec <- function(codec) {
  if (codec %in% c("pcm_alaw", "pcm_mulaw")) {
    return(FALSE)
  }
  return(startsWith(codec, "pcm_") || codec %in% c("flac", "alac", "wavpack", "ape", "tta"))
}
