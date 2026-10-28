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

# A size of 0xFFFFFFFF in the header of a chunk of an RF64 or BW64 file says
# that its real size, too large for the four bytes there, is held in the ds64
# chunk instead.
.WAV_DEFERRED <- 4294967295

# How much of a ds64 chunk is read. It holds three sizes and a table giving the
# size of any other chunk too large for its own header, of which a file has few
# if any, so this too is far more than is needed; it is here so that a table
# declared wrongly, or made wrong on purpose, cannot ask for an unbounded read.
.WAV_DS64_READ <- 65536L

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

#' Report an RF64 or BW64 file whose ds64 chunk cannot be believed
#'
#' An RF64 or BW64 file keeps the sizes a RIFF header has no room for in a ds64
#' chunk, which must come first. Where that chunk is missing, or disagrees with
#' the file, there is no telling which of the sizes is wrong, and walking the
#' chunks by any of them would be a guess. It is raised as an unsupportedWav,
#' so that everything that passes over a file it cannot read passes over this
#' one, and with a class of its own, so that [readAudio()], which reads the
#' audio of such a file all the same, can say what is wrong with it.
#'
#' @param ... Parts of the message
#' @return Nothing, called for the condition it raises
#' @keywords internal
#' @noRd
.badDs64 <- function(...) {
  stop(structure(
    class = c("badDs64", "unsupportedWav", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' Write a size as the whole number it is
#'
#' A size read from a file is a double, which is otherwise printed in
#' scientific notation once it reaches a million or so.
#'
#' @param x A number
#' @return A string
#' @keywords internal
#' @noRd
.wavNumber <- function(x) {
  format(x, scientific=FALSE)
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
#' A recording too long for the four bytes a RIFF header gives a size is written
#' in the RF64 or BW64 form, which keeps its sizes in a ds64 chunk. Such a file
#' is walked by those sizes, and refused where the chunk cannot be believed.
#'
#' @param read A function of the one-based position of the first byte wanted and
#'   the number of bytes to fetch, returning them as a raw vector
#' @param n Number of bytes that can be read
#' @param total Size of the whole file, where only the start of it can be read.
#'   Chunks are only reported as far as `n` goes, but a chunk running past it is
#'   not mistaken for a truncated one.
#' @return A list giving the form of the file ("RIFF", "RF64" or "BW64"), the
#'   RIFF size, which an RF64 or BW64 file gives in its ds64 chunk, that chunk
#'   as .wavDs64Check() reports it, or NULL for a RIFF file, and a list of
#'   chunks, each with its identifier, the one-based position of the first byte
#'   of its body, its size in bytes, and whether it was truncated
#' @keywords internal
#' @noRd
.wavWalk <- function(read, n, total) {
  if (n < 12L) {
    .unsupportedWav("File is too short to be a WAV file.")
  }
  header <- read(1L, 12L)
  form <- .rawToId(header[1:4])
  if (!form %in% c("RIFF", "RF64", "BW64")) {
    .unsupportedWav("Could not find a RIFF, RF64 or BW64 chunk ID.")
  }
  if (!.rawIs(header[9:12], "WAVE")) {
    .unsupportedWav("Could not find WAVE ID.")
  }
  ds64 <- if (form == "RIFF") NULL else .wavDs64(read, n)

  chunks <- list()
  pos <- 13L
  while (pos + 7L <= n) {
    bytes <- read(pos, 8L)
    id <- .rawToId(bytes[1:4])
    size <- .rawToUInt(bytes[5:8])
    if (!is.null(ds64)) {
      size <- .wavDs64Size(ds64, id, size)
    }
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

  if (is.null(ds64)) {
    return(list(
      form=form, riffSize=.rawToUInt(header[5:8]), ds64=NULL, chunks=chunks
    ))
  }
  ds64 <- .wavDs64Check(ds64, chunks, read, n, total)
  return(list(form=form, riffSize=ds64$riffSize, ds64=ds64, chunks=chunks))
}

#' Read the ds64 chunk of an RF64 or BW64 file
#'
#' The chunk holds the RIFF size, the data size and the number of sample frames,
#' each in eight bytes, then a table giving the size of any other chunk too
#' large for the four bytes of its own header. It must be the first chunk in the
#' file, and is not looked for anywhere else: a reader going by the
#' specification would find none, and read the file by other sizes. A chunk of
#' 24 bytes holds the three sizes and no count of table entries, and is read as
#' having no table, as FFmpeg reads it.
#'
#' @param read A function of a one-based position and a number of bytes
#' @param n Number of bytes that can be read
#' @return A list giving the one-based position of the body of the chunk, the
#'   RIFF size, data size and sample count it holds, and its table, as sizes
#'   named by the chunk they are for
#' @keywords internal
#' @noRd
.wavDs64 <- function(read, n) {
  header <- if (n >= 20L) read(13L, 8L) else raw(0)
  if (length(header) < 8L || !.rawIs(header[1:4], "ds64")) {
    .badDs64(
      "The first chunk is not ds64, which an RF64 or BW64 file must begin with."
    )
  }
  size <- .rawToUInt(header[5:8])
  if (size < 24) {
    .badDs64(
      "The ds64 chunk is ", .wavNumber(size),
      " bytes, too short to hold the sizes it is for."
    )
  }
  if (20 + size > n) {
    .badDs64("The ds64 chunk runs past the end of the bytes read.")
  }
  body <- read(21L, min(size, .WAV_DS64_READ))

  entries <- 0
  if (size >= 28) {
    entries <- .rawToUInt(body[25:28])
    if (28 + 12 * entries > size) {
      .badDs64(
        "The ds64 chunk declares a table of ", .wavNumber(entries),
        " entries, which it has no room for."
      )
    }
  }
  # Only the entries that were read are looked at. A chunk whose entry was not
  # read is taken at the size its own header gives, as a chunk with no entry.
  held <- min(entries, max(0, (length(body) - 28) %/% 12))
  at <- 28 + 12 * (seq_len(held) - 1)
  table <- vapply(at, function(i) .rawToUInt(body[(i + 5):(i + 12)]), numeric(1))
  names(table) <- vapply(at, function(i) .rawToId(body[(i + 1):(i + 4)]), character(1))

  return(list(
    start = 21,
    riffSize = .rawToUInt(body[1:8]),
    dataSize = .rawToUInt(body[9:16]),
    sampleCount = .rawToUInt(body[17:24]),
    table = table
  ))
}

#' The size of a chunk of an RF64 or BW64 file
#'
#' A size of 0xFFFFFFFF defers to the ds64 chunk: the data chunk to the data size
#' it gives, and any other chunk to its entry in the table. A chunk the table has
#' no entry for keeps the size it gives, which runs past the end of anything
#' smaller than 4 GiB and is reported as truncated, as it would be in a RIFF
#' file. Audio read as a chunk header after a data chunk whose size was never
#' written can give that size, and it is no reason to refuse the file.
#'
#' A data chunk that gives a size of its own must give the one the ds64 chunk
#' does. FFmpeg and libsndfile read the audio by the ds64 chunk whatever the data
#' chunk says, so where the two disagree, readers disagree about how much audio
#' the file holds.
#'
#' @param ds64 The ds64 chunk, as .wavDs64() reads it
#' @param id The identifier of the chunk
#' @param size The size its header gives
#' @return The size of the chunk
#' @keywords internal
#' @noRd
.wavDs64Size <- function(ds64, id, size) {
  if (id == "data") {
    if (size != .WAV_DEFERRED && size != ds64$dataSize) {
      .badDs64(
        "The data chunk gives a size of ", .wavNumber(size),
        " bytes, and the ds64 chunk a size of ", .wavNumber(ds64$dataSize), "."
      )
    }
    return(ds64$dataSize)
  }
  if (size == .WAV_DEFERRED && id %in% names(ds64$table)) {
    return(ds64$table[[id]])
  }
  return(size)
}

#' Check the sizes a ds64 chunk gives against the file
#'
#' The RIFF size is weighed against the length of the file. A file shorter than
#' it says has been cut short, as a recording is by a flat battery or a full
#' card, which leaves a chunk running past its end; a file as long as it says
#' leaves none. A file longer than it says holds bytes nothing accounts for,
#' which a recorder that stopped before writing its sizes leaves after an empty
#' data chunk. In every other case there is no telling which size is wrong, so
#' the file is refused rather than guessed at.
#'
#' The number of sample frames is weighed as well, but never refused: the audio
#' is read by the data size, and a file whose count is wrong is no harder to
#' read. It can only be weighed for PCM and floating-point audio, where a frame
#' takes a fixed number of bytes, and a count of zero gives none.
#'
#' @param ds64 The ds64 chunk, as .wavDs64() reads it
#' @param chunks The chunks walked
#' @param read A function of a one-based position and a number of bytes
#' @param n Number of bytes that can be read
#' @param total Size of the whole file
#' @return The ds64 chunk, with the bytes a sample frame takes, or NA where that
#'   is not known, and whether its count of frames agrees with its data size
#' @keywords internal
#' @noRd
.wavDs64Check <- function(ds64, chunks, read, n, total) {
  cut <- any(vapply(chunks, function(chunk) chunk$truncated, logical(1)))
  declared <- ds64$riffSize + 8
  if (declared < total && ds64$dataSize != 0) {
    .badDs64(
      "The ds64 chunk gives a RIFF size of ", .wavNumber(ds64$riffSize),
      " bytes, which leaves ", .wavNumber(total - declared),
      " bytes at the end of the file that nothing accounts for."
    )
  }
  if (declared > total && !cut) {
    .badDs64(
      "The ds64 chunk gives a RIFF size of ", .wavNumber(ds64$riffSize),
      " bytes, ", .wavNumber(declared - total), " more than the file holds, ",
      "though nothing in it is cut short."
    )
  }
  if (declared == total && cut) {
    .badDs64("A chunk runs past the end of a file the ds64 chunk says is whole.")
  }

  fields <- NULL
  fmt <- .wavFindChunk(chunks, "fmt ")
  if (!is.null(fmt)) {
    held <- min(fmt$size, 26, n - fmt$start + 1)
    if (held >= 16) {
      fields <- .wavFmtFields(read(fmt$start, held), fmt$size)
    }
  }
  fixed <- .wavFixedFrames(fields)
  ds64$frameSize <- if (fixed) fields$frameSize else NA
  ds64$countAgrees <- ds64$sampleCount == 0 || !fixed ||
    is.null(.wavFindChunk(chunks, "data")) ||
    ds64$sampleCount == floor(ds64$dataSize / fields$frameSize)
  return(ds64)
}

#' Read the fields of a format chunk
#'
#' @param body The body of a fmt chunk, or as much of the start of it as is in
#'   hand
#' @param size The size the chunk declares
#' @return A list giving the format, which for WAVE_FORMAT_EXTENSIBLE is the
#'   format it extends, the number of channels, the sample rate, the bit depth
#'   and the bytes a sample frame takes; or NULL where `body` stops short of a
#'   field that is needed
#' @keywords internal
#' @noRd
.wavFmtFields <- function(body, size) {
  if (length(body) < 16L) {
    return(NULL)
  }
  format <- .rawToUInt(body[1:2])
  # WAVE_FORMAT_EXTENSIBLE, which is what tuneR writes, gives the format it
  # extends in the first two bytes of its sub-format identifier.
  if (format == 65534 && size >= 40) {
    if (length(body) < 26L) {
      return(NULL)
    }
    format <- .rawToUInt(body[25:26])
  }
  channels <- .rawToUInt(body[3:4])
  bits <- .rawToUInt(body[15:16])
  return(list(
    format = format,
    channels = channels,
    sampleRate = .rawToUInt(body[5:8]),
    bits = bits,
    frameSize = channels * ceiling(bits / 8)
  ))
}

#' Does every sample frame of a format take the same number of bytes?
#'
#' Only PCM and floating-point audio can be counted in sample frames from the
#' number of bytes it takes. A compressed format packs its samples into blocks
#' of its own.
#'
#' @param fields The fields of a format chunk, as .wavFmtFields() reads them,
#'   or NULL where they could not be read
#' @return A logical value
#' @keywords internal
#' @noRd
.wavFixedFrames <- function(fields) {
  return(!is.null(fields) && fields$format %in% c(1, 3) && fields$frameSize > 0)
}

#' Where the sizes of a wave file are written, and what to write there
#'
#' A RIFF file holds its sizes in the four bytes of the headers they describe.
#' An RF64 or BW64 file holds them in its ds64 chunk, with 0xFFFFFFFF left in
#' those headers to say so, and holds the number of sample frames there too.
#' That count is given anew with the data size: as the frames the size holds,
#' or as zero, which gives none, where the format does not say how many bytes a
#' frame takes. Giving the data size a file already has therefore puts right a
#' count that disagrees with it.
#'
#' @param form The form of the file, as .wavWalk() reports it
#' @param ds64 The ds64 chunk, as .wavWalk() reports it, or NULL for a RIFF file
#' @param dataStart One-based position of the first byte of audio
#' @param riffSize The RIFF size to write
#' @param dataSize The data size to write, or NULL to leave it as it is
#' @return A list of patches, each giving the one-based position to write at
#'   and the bytes to write there
#' @keywords internal
#' @noRd
.wavSizePatches <- function(form, ds64, dataStart, riffSize, dataSize=NULL) {
  if (form == "RIFF") {
    patches <- list(list(at=5, bytes=.uintToRaw(riffSize, 4L)))
    if (!is.null(dataSize)) {
      patches[[2L]] <- list(at=dataStart - 4, bytes=.uintToRaw(dataSize, 4L))
    }
    return(patches)
  }
  deferred <- .uintToRaw(.WAV_DEFERRED, 4L)
  patches <- list(
    list(at=5, bytes=deferred),
    list(at=ds64$start, bytes=.uintToRaw(riffSize, 8L))
  )
  if (!is.null(dataSize)) {
    frames <- if (is.na(ds64$frameSize)) 0 else floor(dataSize / ds64$frameSize)
    patches[[3L]] <- list(
      at=ds64$start + 8,
      bytes=c(.uintToRaw(dataSize, 8L), .uintToRaw(frames, 8L))
    )
    patches[[4L]] <- list(at=dataStart - 4, bytes=deferred)
  }
  return(patches)
}

#' Write patches into the bytes of a file
#'
#' @param bytes A raw vector holding a file, or as much of the start of one as
#'   the patches fall in
#' @param patches The patches, as .wavSizePatches() gives them
#' @return The bytes, patched
#' @keywords internal
#' @noRd
.wavPatch <- function(bytes, patches) {
  for (patch in patches) {
    bytes[patch$at:(patch$at + length(patch$bytes) - 1)] <- patch$bytes
  }
  return(bytes)
}

#' Warn that a ds64 chunk miscounts the sample frames in a file
#'
#' @param ds64 The ds64 chunk, as .wavWalk() reports it
#' @param file The file it was read from
#' @param corrected TRUE where the count has been put right in a file written
#' @return NULL
#' @keywords internal
#' @noRd
.wavCountWarning <- function(ds64, file, corrected=FALSE) {
  .audioFileWarning(
    "frameCountMismatch",
    "The ds64 chunk of '", file, "' gives ", .wavNumber(ds64$sampleCount),
    if (ds64$sampleCount == 1) " sample frame" else " sample frames",
    ", where its data holds ",
    .wavNumber(floor(ds64$dataSize / ds64$frameSize)), ".",
    if (corrected) " The count has been corrected." else ""
  )
  invisible(NULL)
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

#' Could a run of bytes be the start of a RIFF chunk?
#'
#' Used to tell a chunk written after the audio from audio whose size the writer
#' never recorded.
#'
#' @param bytes The eight bytes where a chunk header would be
#' @param available Number of bytes from there to the end of the file
#' @return A logical value
#' @keywords internal
#' @noRd
.wavIsChunkHeader <- function(bytes, available) {
  if (length(bytes) < 8L) {
    return(FALSE)
  }
  id <- as.integer(bytes[1:4])
  if (any(id < 32L | id > 126L)) {
    return(FALSE)
  }
  return(8 + .rawToUInt(bytes[5:8]) <= available)
}

#' How far does a data chunk whose size was never written run?
#'
#' A recorder that loses power part-way through a file never goes back to write
#' the size of the audio, leaving an empty data chunk with the recording still in
#' the file. Where bytes follow such a chunk that the RIFF header does not
#' account for, and that do not begin a chunk of their own, they are audio the
#' writer never recorded and the chunk runs to the end of the file. A file that
#' really holds no audio, as an AudioMoth writes when its battery fails as it
#' starts to record, declares its own length correctly and is left alone.
#'
#' @param data The data chunk, as .wavWalk() reports it
#' @param riffSize The size recorded in the RIFF header
#' @param blockAlign Bytes per sample frame
#' @param total Size of the whole file
#' @param read A function of a one-based position and a number of bytes, or NULL
#'   where the bytes after the chunk header cannot be read
#' @return The size the audio runs to, a whole number of sample frames, or 0
#'   where the chunk holds no audio
#' @keywords internal
#' @noRd
.wavUnfinalisedSize <- function(data, riffSize, blockAlign, total, read=NULL) {
  if (data$size != 0 || blockAlign <= 0) {
    return(0)
  }
  following <- total - data$start + 1
  # Bytes the RIFF header accounts for are a chunk the writer meant to be there.
  if (following < blockAlign || riffSize + 8 >= total) {
    return(0)
  }
  if (!is.null(read) && .wavIsChunkHeader(read(data$start, 8L), following)) {
    return(0)
  }
  return(following - following %% blockAlign)
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
#'   of audio data, the size of the data chunk, whether that size was never
#'   written, the size recorded in the RIFF header, and the form of the file and
#'   its ds64 chunk, as .wavWalk() reports them
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
  # Where `bytes` holds only the start of the file, a chunk that fits inside
  # the file is not necessarily held in them. Reading past the end of a raw
  # vector gives zero bytes rather than failing, so the fields would be read as
  # a format, channel count or bit depth of zero and the file turned away for a
  # reason that is not true of it. Saying that the bytes fall outside what was
  # read lets the caller read more of the file and ask again, which is what
  # audiomothExpand() does, so such a file is read rather than refused.
  held <- function(through) at + through <= length(bytes)
  if (!held(15L)) {
    .unsupportedWav("FMT chunk lies beyond the bytes read.")
  }
  fields <- .wavFmtFields(bytes[at:(at + if (held(25L)) 25L else 15L)], fmt$size)
  if (is.null(fields)) {
    .unsupportedWav("FMT chunk sub-format lies beyond the bytes read.")
  }

  # The bytes after the data chunk header can only be weighed where they were
  # read; without them the size the RIFF header declares has to answer on its
  # own.
  peek <- if (data$start + 7L <= length(bytes)) {
    function(at, n) bytes[at:(at + n - 1L)]
  } else {
    NULL
  }
  unfinalised <- .wavUnfinalisedSize(
    data, info$riffSize, fields$frameSize, total, peek
  )

  return(list(
    format = fields$format,
    channels = fields$channels,
    sampleRate = fields$sampleRate,
    bits = fields$bits,
    dataStart = data$start,
    dataSize = if (unfinalised > 0) unfinalised else data$size,
    dataTruncated = data$truncated,
    dataUnfinalised = unfinalised > 0,
    riffSize = info$riffSize,
    form = info$form,
    ds64 = info$ds64
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

#' Copy a run of bytes from one file to another in bounded buffers
#'
#' Written a buffer at a time so that the memory needed does not grow with the
#' length of the recording being copied.
#'
#' @param read A function of the one-based position of the first byte wanted and
#'   the number of bytes to fetch, returning them as a raw vector, as
#'   .wavConReader() gives
#' @param con A binary connection to write to
#' @param from One-based position of the first byte to copy
#' @param to One-based position of the last byte to copy. Nothing is copied
#'   where it comes before `from`, which is how a run of no bytes is asked for.
#' @return NULL
#' @keywords internal
#' @noRd
.copyBytes <- function(read, con, from, to) {
  bufferSize <- 2^20
  # Counted in doubles, as a recording of 2GB or more is longer than a whole
  # number can be counted in.
  at <- as.numeric(from)
  while (at <= to) {
    n <- min(bufferSize, to - at + 1)
    writeBin(read(at, n), con)
    at <- at + n
  }
  invisible(NULL)
}

#' Copy a run of bytes, replacing some of them as it goes
#'
#' Used to carry a recording across while putting right the few bytes that have
#' to change, the sizes a file declares, without holding the recording in
#' memory or going back over what has been written.
#'
#' @param read A reader, as .copyBytes() takes
#' @param con A binary connection to write to
#' @param from One-based position of the first byte to copy
#' @param to One-based position of the last byte to copy
#' @param edits A list of replacements, each a list of `at`, the one-based
#'   position in the source of the first byte it replaces, and `bytes`, the raw
#'   vector to write there. An edit lying outside the run is left for whichever
#'   run holds it, and one lying across its end is written as far as it goes.
#'   Where two edits cover the same byte, the earlier of them writes it.
#' @return NULL
#' @keywords internal
#' @noRd
.copyEdited <- function(read, con, from, to, edits=list()) {
  at <- as.numeric(from)
  where <- vapply(edits, function(edit) as.numeric(edit$at), numeric(1))
  for (edit in edits[order(where)]) {
    first <- as.numeric(edit$at)
    last <- first + length(edit$bytes) - 1
    # An edit before where the copy has reached, or past the end of the run,
    # belongs to another run: this one has written past it, or stops short of
    # it. Writing it here would send the copy backwards over what it has put
    # out already.
    if (last < at || first > to || at > to) {
      next
    }
    .copyBytes(read, con, at, min(first, to + 1) - 1)
    inside <- seq(max(first, at), min(last, to))
    writeBin(edit$bytes[inside - first + 1], con)
    at <- min(last, to) + 1
  }
  .copyBytes(read, con, at, to)
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
  state <- new.env(parent=emptyenv())
  state$open <- TRUE

  # However this call ends, the connection is closed and the part written file
  # removed, and in that order, as a file still open cannot be removed on some
  # systems. It is done here rather than where an error is caught because
  # someone cancelling the call raises something no error handler sees, and
  # that would leave the connection open and a part of a recording behind.
  on.exit({
    if (state$open) {
      suppressWarnings(try(close(con), silent=TRUE))
      state$open <- FALSE
    }
    unlink(part)
  }, add=TRUE)

  if (file.exists(path) && !isTRUE(Sys.chmod(part, file.info(path)$mode))) {
    stop("Could not preserve file permissions for: ", path)
  }

  withCallingHandlers({
    write(con)
    close(con)
    state$open <- FALSE
  }, warning = complain)

  if (!file.rename(part, path)) {
    stop("Could not put the written file in place of: ", path)
  }
  return(invisible(path))
}
