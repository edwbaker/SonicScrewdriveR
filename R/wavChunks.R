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
  x <- as.numeric(x)
  # Taking the value modulo the field would quietly write a wrapped size, which
  # makes a file that no reader can make sense of.
  if (is.na(x) || x < 0 || x >= 256^size) {
    stop(
      "A value of ", format(x, scientific=FALSE),
      " does not fit in the ", size, " bytes a WAV header allows for it."
    )
  }
  return(as.raw(floor(x / 256^(seq_len(size) - 1L)) %% 256))
}

# How much of an INFO list is read to find the comment in it. The list a
# recorder writes is a few hundred bytes, the one AudioMoth writes 436, so this
# is far more than any of them need; it is here so that a size read from a file
# that is wrong, or made wrong on purpose, cannot ask for an unbounded read.
.WAV_INFO_READ <- 65536L

#' Find the ICMT field within the body of an INFO list
#'
#' @param body The body of a LIST chunk
#' @return The comment, as the bytes it is written in, or ""
#' @keywords internal
#' @noRd
.infoComment <- function(body) {
  if (length(body) < 4L || !.rawIs(body[1:4], "INFO")) {
    return("")
  }
  pos <- 5L
  while (pos + 7L <= length(body)) {
    size <- .rawToUInt(body[(pos + 4L):(pos + 7L)])
    at <- pos + 8L
    if (.rawIs(body[pos:(pos + 3L)], "ICMT")) {
      end <- min(at + size - 1, length(body))
      if (end < at) {
        return("")
      }
      held <- body[at:end]
      # Read as bytes rather than as characters of any encoding: the field is
      # padded with nothing in particular and may hold a part of a character
      # where a writer has cut it short.
      return(rawToChar(held[as.integer(held) != 0L]))
    }
    pos <- at + size + (size %% 2)
  }
  return("")
}

#' Read the comment a wave file carries, wherever it is held
#'
#' The comment is the ICMT field of the INFO list, which is where a recorder
#' writes what it has to say about a recording. Other fields of that list, and
#' other chunks, hold text of their own that says nothing about the recording,
#' so the comment is read as the one field rather than looked for anywhere in
#' the file.
#'
#' The chunks are walked rather than a fixed run of bytes read, as nothing
#' fixes where the list sits: writing metadata can move it, and a comment
#' pushed past wherever a reader stopped looking would make a recording out to
#' be something it is not.
#'
#' @param file Path to a wave file
#' @return The comment, as the bytes it is written in, or "" where there is
#'   none to be found
#' @keywords internal
#' @noRd
.wavCommentFromFile <- function(file) {
  size <- file.size(file)
  if (is.na(size) || size < 12) {
    return("")
  }
  con <- file(file, "rb")
  on.exit(close(con))
  read <- .wavConReader(con)
  info <- tryCatch(.wavWalk(read, size, size), unsupportedWav=function(cond) NULL)
  if (is.null(info)) {
    return("")
  }
  # A file may hold more than one list: one of cue labels and one of
  # information, in either order, and a list of information need not hold a
  # comment. Taking the first and looking no further would miss the comment
  # behind any of those, and a triggered recording whose comment was missed is
  # read as the shortened thing on disk.
  for (chunk in info$chunks) {
    if (chunk$id != "LIST" || chunk$size < 4) {
      next
    }
    comment <- .infoComment(read(chunk$start, min(chunk$size, .WAV_INFO_READ)))
    if (nzchar(comment)) {
      return(comment)
    }
  }
  return("")
}

#' Check that an argument is one value and not several
#'
#' Several of these arguments name a file, or go into a field of a header that
#' holds one value. Given a vector, the code reading them compares or writes a
#' piece at a time and fails in ways that say nothing about what was given, so
#' they are checked where they come in.
#'
#' @param x The value given
#' @param name What to call it
#' @param numeric TRUE to require a number rather than a string
#' @return The value, as one string or number
#' @keywords internal
#' @noRd
.oneValue <- function(x, name, numeric=FALSE) {
  if (length(x) != 1 || is.na(x)) {
    stop(name, " must be a single ", if (numeric) "number." else "string.")
  }
  if (numeric && !is.numeric(x)) {
    stop(name, " must be a single number.")
  }
  return(if (numeric) as.numeric(x) else as.character(x))
}

#' Report a file this package cannot treat as an AudioMoth recording
#'
#' Raised where a file is not a wave file at all, or is not the mono 16-bit PCM
#' an AudioMoth device writes. [readAudio()] reads such a file by its ordinary
#' means, so it is told apart from a failure to expand a recording that really
#' is a triggered one, which must not be passed over.
#'
#' @param ... Parts of the message
#' @return Nothing, called for the condition it raises
#' @keywords internal
#' @noRd
.unsupportedWav <- function(...) {
  stop(structure(
    class = c("unsupportedWav", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
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

#' Walk the RIFF chunks of a WAV file
#'
#' The bytes are fetched through a function given to it, so that a file held in
#' memory and a file read over a connection cannot come to disagree about the
#' chunks it holds.
#'
#' A chunk whose size runs past the end of the file is reported with the size it
#' actually has and flagged as truncated, rather than failing, as recordings cut
#' short by a flat battery or a full card are common.
#'
#' @param read A function of the one-based position of the first byte wanted and
#'   the number of bytes to fetch, returning them as a raw vector
#' @param n Number of bytes that can be read
#' @param total Size of the whole file, where only the start of it can be read.
#'   Chunks are only reported as far as `n` goes, but a chunk running past it is
#'   not mistaken for a truncated one.
#' @return A list giving the size recorded in the RIFF header and a list of
#'   chunks, each with its identifier, the one-based position of the first byte
#'   of its body, its size in bytes, and whether it was truncated
#' @keywords internal
#' @noRd
.wavWalk <- function(read, n, total) {
  if (n < 12L) {
    .unsupportedWav("File is too short to be a WAV file.")
  }
  header <- read(1L, 12L)
  if (!.rawIs(header[1:4], "RIFF")) {
    .unsupportedWav("Could not find RIFF chunk ID.")
  }
  if (!.rawIs(header[9:12], "WAVE")) {
    .unsupportedWav("Could not find WAVE ID.")
  }

  chunks <- list()
  pos <- 13L
  while (pos + 7L <= n) {
    bytes <- read(pos, 8L)
    id <- .rawToId(bytes[1:4])
    size <- .rawToUInt(bytes[5:8])
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

  return(list(riffSize=.rawToUInt(header[5:8]), chunks=chunks))
}

#' List the RIFF chunks of a WAV file held as raw bytes
#'
#' @param bytes A raw vector holding a WAV file, or the start of one
#' @param total Size of the whole file, where `bytes` holds only the start of it
#' @return The chunks, as .wavWalk() reports them
#' @keywords internal
#' @noRd
.wavChunks <- function(bytes, total=length(bytes)) {
  read <- function(at, n) bytes[at:(at + n - 1L)]
  return(.wavWalk(read, length(bytes), total))
}

#' Fetch a run of bytes from a connection by position
#'
#' @param con A binary connection that can seek
#' @return A function of a one-based position and a number of bytes
#' @keywords internal
#' @noRd
.wavConReader <- function(con) {
  function(at, n) {
    seek(con, at - 1)
    return(readBin(con, "raw", n=n))
  }
}

#' List the RIFF chunks of a WAV file without reading the whole file
#'
#' Reads each chunk header and seeks past the body it describes, so that the
#' memory needed does not grow with the length of the recording.
#'
#' @param file Path to a WAV file
#' @return The chunks, as .wavWalk() reports them
#' @keywords internal
#' @noRd
.wavChunksFile <- function(file) {
  con <- file(file, "rb")
  on.exit(close(con))
  size <- file.size(file)
  return(.wavWalk(.wavConReader(con), size, size))
}

#' Read the body of one chunk of a WAV file without reading the whole file
#'
#' @param file Path to a WAV file
#' @param id The chunk identifier to look for
#' @return A raw vector holding the body of the chunk, or NULL where the file
#'   has no such chunk or it holds nothing
#' @keywords internal
#' @noRd
.wavChunkFromFile <- function(file, id) {
  con <- file(file, "rb")
  on.exit(close(con))
  size <- file.size(file)
  read <- .wavConReader(con)
  chunk <- .wavFindChunk(.wavWalk(read, size, size)$chunks, id)
  if (is.null(chunk) || chunk$size < 1) {
    return(NULL)
  }
  return(read(chunk$start, chunk$size))
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
    .unsupportedWav("Could not find FMT chunk ID.")
  }
  if (is.null(data)) {
    .unsupportedWav("Could not find DATA chunk ID.")
  }
  if (fmt$size < 16) {
    .unsupportedWav("FMT chunk is too short.")
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

#' Cut text to fit a fixed number of bytes, keeping whole characters
#'
#' A field of a WAV header holds a set number of bytes, and a character may
#' take several of them. Cutting at the byte would leave a part of a character
#' at the end, which is no longer text: it cannot be compared, written back out
#' or read, and raises rather than giving the shortened comment that was meant.
#'
#' @param string The text to cut
#' @param bytes How many bytes it may take
#' @return The longest run of whole characters that fits
#' @keywords internal
#' @noRd
.cutToBytes <- function(string, bytes) {
  # Counted as the bytes it will be written as, whatever encoding it arrived in.
  string <- enc2utf8(as.character(string))
  characters <- strsplit(string, "")[[1]]
  fits <- cumsum(nchar(characters, type="bytes")) <= bytes
  return(paste0(characters[fits], collapse=""))
}

#' Write a file without destroying what is already there
#'
#' Opening a file to write empties it, so a write that fails part way through
#' leaves nothing of what was there before. That matters most where a file is
#' written over itself: putting one field of metadata into a recording should
#' not be able to lose the recording.
#'
#' The bytes go to a file beside the one being written, which takes its place
#' only once it is whole. The two are in the same directory so that they are on
#' the same volume, where taking its place is a rename rather than a copy.
#'
#' @param path Where the file is to end up
#' @param write A function given a connection to write to
#' @return The path (invisibly)
#' @keywords internal
#' @noRd
.writeAtomically <- function(path, write) {
  part <- tempfile(tmpdir=dirname(path), fileext=".part")
  on.exit(unlink(part), add=TRUE)

  # A write that stops short, on a disk with no room left for instance, is
  # reported by a warning rather than an error. Output that stops short must
  # not take the place of a recording that is whole, so a warning while
  # writing or closing is taken as the failure it is.
  complain <- function(condition) {
    stop(
      "Writing ", path, " did not finish: ", conditionMessage(condition),
      call. = FALSE
    )
  }
  con <- file(part, "wb")
  tryCatch(
    withCallingHandlers({
      write(con)
      close(con)
    }, warning = complain),
    error = function(condition) {
      suppressWarnings(try(close(con), silent=TRUE))
      stop(condition)
    }
  )

  if (!file.rename(part, path)) {
    stop("Could not put the written file in place of: ", path)
  }
  return(invisible(path))
}
