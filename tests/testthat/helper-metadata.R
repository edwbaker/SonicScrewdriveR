# Builders for the metadata the readers are tested against.
#
# The files are built here rather than committed, so that what each test is
# about is written out where the test is: a committed file would say only that
# the reader still agrees with whatever made it. Each builder writes the bytes
# the format specifies, by hand, so that a reader cannot be tested against the
# same mistake that produced its input.

#' Little-endian unsigned integer as bytes
mdU32 <- function(x) as.raw(floor(as.numeric(x) / 256^(0:3)) %% 256)

#' Little-endian unsigned short as bytes
mdU16 <- function(x) as.raw(floor(as.numeric(x) / 256^(0:1)) %% 256)

#' Big-endian unsigned integer as bytes, as ID3 writes a size
mdBE32 <- function(x) rev(mdU32(x))

#' Big-endian three-byte size, as ID3v2.2 and FLAC write one
mdBE24 <- function(x) rev(mdU32(x))[2:4]

#' ID3 sync-safe size, seven bits to the byte
mdSyncsafe <- function(x) {
  as.raw(c(
    floor(x / 128^3) %% 128, floor(x / 128^2) %% 128,
    floor(x / 128) %% 128, x %% 128
  ))
}

#' Text as the UTF-8 bytes a format that declares that encoding holds
mdUtf8 <- function(text) charToRaw(enc2utf8(text))

#' Text as the UTF-16 bytes, little end first, that a clipboard item holds
mdUtf16 <- function(text) {
  codes <- utf8ToInt(enc2utf8(text))
  return(as.raw(c(rbind(codes %% 256L, codes %/% 256L))))
}

#' Text as a run of bytes of a fixed width, padded with nuls
mdPad <- function(text, n) {
  bytes <- charToRaw(text)
  return(c(bytes, raw(max(0, n - length(bytes))))[seq_len(n)])
}

#' A RIFF chunk, padded to an even length as the format requires
mdChunk <- function(id, body) {
  if (length(body) %% 2L == 1L) {
    body <- c(body, as.raw(0))
  }
  return(c(charToRaw(id), mdU32(length(body)), body))
}

#' A wave file holding the given chunks
mdWav <- function(...) {
  body <- c(charToRaw("WAVE"), ...)
  return(c(charToRaw("RIFF"), mdU32(length(body)), body))
}

#' A format chunk describing mono 16-bit audio at the given sample rate
mdFmt <- function(rate=48000) {
  return(mdChunk("fmt ", c(
    mdU16(1), mdU16(1), mdU32(rate), mdU32(rate * 2), mdU16(2), mdU16(16)
  )))
}

#' A GUANO chunk holding the given fields, one to a line
mdGuanoChunk <- function(...) {
  return(mdChunk(
    "guan", charToRaw(paste(c("GUANO|Version:1.0", ...), collapse="\n"))
  ))
}

#' A data chunk holding silence
mdData <- function(n=16L) mdChunk("data", raw(n))

#' Write bytes to a file that goes when the test does
mdFile <- function(bytes, extension=".wav") {
  path <- withr::local_tempfile(fileext=extension, .local_envir=parent.frame())
  writeBin(bytes, path)
  return(path)
}

#' One field of a wamd chunk
mdWamdField <- function(id, value) {
  body <- if (is.raw(value)) value else charToRaw(value)
  return(c(mdU16(id), mdU32(length(body)), body))
}

#' One field of a RIFF list, nul-terminated and padded as the format requires
mdInfoField <- function(id, text) {
  body <- c(charToRaw(text), as.raw(0))
  if (length(body) %% 2L == 1L) {
    body <- c(body, as.raw(0))
  }
  return(c(charToRaw(id), mdU32(nchar(text, type="bytes") + 1L), body))
}

#' One field of an associated data list, which begins with the cue point it
#' belongs to and, for a labelled text field, a header besides
mdCueField <- function(id, cue, text, header=4L) {
  body <- c(
    mdU32(cue),
    raw(max(0L, header - 4L)),
    if (nzchar(text)) c(charToRaw(text), as.raw(0)) else raw(0)
  )
  # The size a field records is that of its body before the padding that
  # brings the field to an even length.
  size <- length(body)
  if (length(body) %% 2L == 1L) {
    body <- c(body, as.raw(0))
  }
  return(c(charToRaw(id), mdU32(size), body))
}

#' A bext chunk, with the description and originator given
mdBextChunk <- function(description, originator="An Originator",
                        date="2011-05-23", time="14:22:03",
                        history="A=PCM,F=48000,W=24,M=mono") {
  return(mdChunk("bext", c(
    mdPad(description, 256L),
    mdPad(originator, 32L),
    mdPad("REF123", 32L),
    mdPad(date, 10L),
    mdPad(time, 8L),
    mdU32(0), mdU32(0),   # the sample the recording starts at, in two halves
    mdU16(1),             # the version of the chunk
    raw(64),              # the unique material identifier, unused
    raw(190),             # loudness, and the bytes reserved for later use
    charToRaw(history)
  )))
}

#' An ID3v2.4 frame, whose size is that of the body as it is stored
mdFrame24 <- function(id, body, flags=raw(2)) {
  return(c(charToRaw(id), mdSyncsafe(length(body)), flags, body))
}

#' An ID3v2.3 frame, whose size is written plainly rather than sync-safe
mdFrame23 <- function(id, body) {
  return(c(charToRaw(id), mdBE32(length(body)), raw(2), body))
}

#' An ID3v2.2 frame, named by three characters with a six-byte header
mdFrame22 <- function(id, body) {
  return(c(charToRaw(id), mdBE24(length(body)), body))
}

#' An ID3v2 text frame
mdTextFrame <- function(id, text, encoding=0L) {
  body <- c(as.raw(encoding), charToRaw(text))
  return(if (nchar(id) == 3L) mdFrame22(id, body) else mdFrame24(id, body))
}

#' An ID3v2 tag of a given version holding the given frames
mdId3Tag <- function(body, major=4L, flags=0L) {
  return(c(
    charToRaw("ID3"), as.raw(c(major, 0L, flags)),
    mdSyncsafe(length(body)), body
  ))
}

#' Unsynchronise a run of bytes, as a tag written that way is held
mdUnsync <- function(bytes) {
  out <- raw(0)
  for (i in seq_along(bytes)) {
    out <- c(out, bytes[i])
    if (as.integer(bytes[i]) == 255L) {
      out <- c(out, as.raw(0))
    }
  }
  return(out)
}

#' The four bytes an MP3 frame header starts with
mdMp3Frame <- function() as.raw(c(255, 251, 144, 0))

#' A Vorbis comment block
mdVorbisBlock <- function(vendor, comments) {
  one <- function(text) c(mdU32(nchar(text, type="bytes")), charToRaw(text))
  return(c(
    mdU32(nchar(vendor, type="bytes")), charToRaw(vendor),
    mdU32(length(comments)), unlist(lapply(comments, one))
  ))
}

#' A FLAC file holding a Vorbis comment block
mdFlac <- function(block) {
  return(c(
    charToRaw("fLaC"),
    as.raw(0), mdBE24(34), raw(34),                 # the stream information
    as.raw(4), mdBE24(length(block)), block,        # the comments
    as.raw(128 + 1), mdBE24(8), raw(8)              # padding, the last block
  ))
}
