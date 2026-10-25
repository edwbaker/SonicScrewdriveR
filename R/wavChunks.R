# Helpers for reading and writing the RIFF chunk structure of WAV files, shared
# by the AudioMoth triggered file format and GUANO metadata functions.

#' Little-endian unsigned integer from raw bytes
#'
#' Returns a double, as WAV chunk sizes can exceed the range of an R integer.
#' @param bytes A raw vector
#' @return A number
#' @keywords internal
#' @noRd
.rawToUInt <- function(bytes) {
  sum(as.numeric(as.integer(bytes)) * 256^(seq_along(bytes) - 1L))
}

#' Little-endian unsigned integer as raw bytes
#'
#' @param x A non-negative number
#' @param size Number of bytes to use
#' @return A raw vector
#' @keywords internal
#' @noRd
.uintToRaw <- function(x, size) {
  as.raw(floor(as.numeric(x) / 256^(seq_len(size) - 1L)) %% 256)
}

#' Does a run of bytes hold a given ASCII identifier?
#'
#' @param bytes A raw vector
#' @param id An identifier
#' @return A logical value
#' @keywords internal
#' @noRd
.rawIs <- function(bytes, id) {
  identical(bytes, charToRaw(id))
}

#' Read a chunk identifier, replacing anything unprintable
#'
#' rawToChar() stops on an embedded nul, which a corrupt file can easily
#' produce where an identifier is expected.
#' @param bytes A raw vector of four bytes
#' @return A string
#' @keywords internal
#' @noRd
.rawToId <- function(bytes) {
  v <- as.integer(bytes)
  v[v < 32L | v > 126L] <- 63L
  rawToChar(as.raw(v))
}

#' List the RIFF chunks of a WAV file
#'
#' A chunk whose size runs past the end of the file is reported with the size it
#' actually has and flagged as truncated, rather than failing, as recordings cut
#' short by a flat battery or a full card are common.
#'
#' @param bytes A raw vector holding a WAV file, or the start of one
#' @param total Size of the whole file, where `bytes` holds only the start of
#'   it. Chunks are only reported as far as `bytes` goes, but a chunk running
#'   past it is not mistaken for a truncated one.
#' @return A list giving the size recorded in the RIFF header and a list of
#'   chunks, each with its identifier, the one-based position of the first byte
#'   of its body, its size in bytes, and whether it was truncated
#' @keywords internal
#' @noRd
.wavChunks <- function(bytes, total=length(bytes)) {
  n <- length(bytes)
  if (n < 12L) {
    stop("File is too short to be a WAV file.")
  }
  if (!.rawIs(bytes[1:4], "RIFF")) {
    stop("Could not find RIFF chunk ID.")
  }
  if (!.rawIs(bytes[9:12], "WAVE")) {
    stop("Could not find WAVE ID.")
  }

  chunks <- list()
  pos <- 13L
  while (pos + 7L <= n) {
    id <- .rawToId(bytes[pos:(pos + 3L)])
    size <- .rawToUInt(bytes[(pos + 4L):(pos + 7L)])
    body <- pos + 8L
    truncated <- body + size - 1 > total
    if (truncated) {
      size <- total - body + 1
    }
    chunks[[length(chunks) + 1L]] <- list(
      id = id, start = body, size = size, truncated = truncated
    )
    if (truncated) {
      break
    }
    # RIFF chunk bodies are padded to an even number of bytes.
    pos <- body + size + (size %% 2)
  }

  return(list(riffSize=.rawToUInt(bytes[5:8]), chunks=chunks))
}

#' Find one chunk of a WAV file by identifier
#'
#' @param chunks The chunk list from .wavChunks()
#' @param id The identifier to look for
#' @return The chunk, or NULL if the file has no such chunk
#' @keywords internal
#' @noRd
.wavFindChunk <- function(chunks, id) {
  for (chunk in chunks) {
    if (chunk$id == id) {
      return(chunk)
    }
  }
  return(NULL)
}

#' Read the format parameters and data position of a WAV file
#'
#' Walks the chunks rather than assuming a fixed header length, as the AudioMoth
#' application note warns that the lengths of the comment and artist fields may
#' change in future firmware, and current firmware writes a GUANO chunk after
#' the audio data.
#'
#' @param bytes A raw vector holding a WAV file, or enough of the start of one
#'   to cover its format and data chunk headers
#' @param total Size of the whole file, where `bytes` holds only the start of it
#' @return A list of format parameters, the one-based position of the first byte
#'   of audio data, the size of the data chunk, and the size recorded in the
#'   RIFF header
#' @keywords internal
#' @noRd
.wavFormat <- function(bytes, total=length(bytes)) {
  info <- .wavChunks(bytes, total=total)
  fmt <- .wavFindChunk(info$chunks, "fmt ")
  data <- .wavFindChunk(info$chunks, "data")
  if (is.null(fmt)) {
    stop("Could not find FMT chunk ID.")
  }
  if (is.null(data)) {
    stop("Could not find DATA chunk ID.")
  }
  if (fmt$size < 16) {
    stop("FMT chunk is too short.")
  }

  at <- fmt$start
  format <- .rawToUInt(bytes[at:(at + 1L)])
  # WAVE_FORMAT_EXTENSIBLE, which is what tuneR writes, gives the format it
  # extends in the first two bytes of its sub-format identifier.
  if (format == 65534 && fmt$size >= 40) {
    format <- .rawToUInt(bytes[(at + 24L):(at + 25L)])
  }

  return(list(
    format = format,
    channels = .rawToUInt(bytes[(at + 2L):(at + 3L)]),
    sampleRate = .rawToUInt(bytes[(at + 4L):(at + 7L)]),
    bits = .rawToUInt(bytes[(at + 14L):(at + 15L)]),
    dataStart = data$start,
    dataSize = data$size,
    dataTruncated = data$truncated,
    riffSize = info$riffSize
  ))
}

#' Write a run of zero bytes to a connection
#'
#' Written in bounded chunks so that a long silent period does not have to be
#' held in memory all at once.
#' @param con A binary connection
#' @param nbytes Number of zero bytes to write
#' @return NULL
#' @keywords internal
#' @noRd
.writeZeros <- function(con, nbytes) {
  chunkSize <- 2^20
  buffer <- raw(min(nbytes, chunkSize))
  while (nbytes > 0) {
    size <- min(nbytes, chunkSize)
    writeBin(if (size == length(buffer)) buffer else raw(size), con)
    nbytes <- nbytes - size
  }
  invisible(NULL)
}
