# Helpers shared by the readers of embedded audio metadata.
#
# None of these readers reads a whole file. Each one finds the bytes that hold
# the metadata and reads only those: a recording may run to hundreds of
# megabytes and sit on a network share, where reading it through to recover a
# few hundred bytes of text costs far more than the metadata is worth.

#' Most bytes read from one metadata block
#'
#' Metadata runs to a few kilobytes in all but pathological cases, but an ID3
#' tag or an INFO list may carry embedded artwork, and a file damaged in the
#' right place can declare a block of any size at all. Reading is bounded so
#' that neither costs more than the metadata is worth.
#' @keywords internal
#' @noRd
.MD_BLOCK_MAX <- 4194304L

#' Bytes read from the start of a file to identify it and find its header
#' @keywords internal
#' @noRd
.MD_HEAD_READ <- 65536L

#' The name a wave keeps the metadata of its file under
#'
#' One name, whether it is an attribute on a plain Wave or an entry in the
#' metadata slot of a TaggedWave, so that there is one thing to look for.
#' @keywords internal
#' @noRd
.MD_STORE <- "audioMetadata"

#' Readable names for the formats these readers read
#' @keywords internal
#' @noRd
.MD_FORMAT_NAMES <- list(
  id3 = "ID3",
  wamd = "wamd",
  bext = "bext",
  wavInfo = "INFO",
  ixml = "iXML",
  vorbisComment = "Vorbis comment"
)

#' Big-endian unsigned integer from raw bytes
#'
#' ID3 holds its sizes big-endian, where RIFF holds them little-endian. Returns
#' a double, as a size can exceed the range of an R integer.
#'
#' @param bytes A raw vector
#' @return A number
#' @keywords internal
#' @noRd
.rawToUIntBE <- function(bytes) {
  n <- length(bytes)
  if (n == 0L) {
    return(0)
  }
  return(sum(as.numeric(as.integer(bytes)) * 256^((n - 1L):0)))
}

#' Unsigned integer from an ID3 sync-safe size
#'
#' ID3v2.4 writes the size of the tag, and of every frame in it, as four bytes
#' carrying seven bits each, so that no byte of a size can be mistaken for the
#' start of an MP3 frame. The top bit of each byte is therefore not part of the
#' number, and is dropped rather than trusted: a writer that sets it has
#' written a size no reader can agree on.
#'
#' @param bytes A raw vector
#' @return A number
#' @keywords internal
#' @noRd
.rawToSyncsafe <- function(bytes) {
  n <- length(bytes)
  if (n == 0L) {
    return(0)
  }
  return(sum(as.numeric(as.integer(bytes) %% 128L) * 128^((n - 1L):0)))
}

#' Is an encoding name one of the UTF-16 encodings?
#' @keywords internal
#' @noRd
.mdIsUtf16 <- function(from) {
  return(grepl("^UTF-16", from))
}

#' Drop the nul bytes that pad a run of text
#'
#' A nul is padding in a single-byte encoding and goes wherever it appears. In
#' UTF-16 it is half of a character, so only whole nul units at the end are
#' dropped, two bytes at a time to keep the units aligned.
#'
#' @param bytes A raw vector
#' @param from The encoding the bytes are declared to be in
#' @return The bytes, without their padding
#' @keywords internal
#' @noRd
.mdStripNuls <- function(bytes, from="latin1") {
  if (length(bytes) == 0L) {
    return(bytes)
  }
  if (.mdIsUtf16(from)) {
    while (length(bytes) >= 2L &&
           all(as.integer(bytes[(length(bytes) - 1L):length(bytes)]) == 0L)) {
      bytes <- bytes[seq_len(length(bytes) - 2L)]
    }
    return(bytes)
  }
  return(bytes[as.integer(bytes) != 0L])
}

#' Are a run of bytes valid UTF-8 holding something above ASCII?
#'
#' Used to tell text that has been declared as ISO-8859-1 but written as UTF-8
#' from text that really is ISO-8859-1. A lone high byte, which is what
#' ISO-8859-1 text gives, is not valid UTF-8, so the test does not misfire on
#' it, and text that is all ASCII reads the same either way and is left alone.
#'
#' @param bytes A raw vector
#' @return A logical value
#' @keywords internal
#' @noRd
.mdIsUtf8 <- function(bytes) {
  if (!any(as.integer(bytes) >= 128L)) {
    return(FALSE)
  }
  decoded <- tryCatch(
    iconv(list(bytes), from="UTF-8", to="UTF-8"),
    error=function(cond) NA_character_
  )
  return(!is.na(decoded[1]))
}

#' Turn a run of bytes into a string of a stated encoding
#'
#' @param bytes A raw vector holding no nul padding
#' @param from The encoding to read them in
#' @return A string, valid UTF-8 whatever the bytes held
#' @keywords internal
#' @noRd
.mdIconv <- function(bytes, from) {
  text <- tryCatch(
    iconv(list(bytes), from=from, to="UTF-8", sub="byte"),
    error=function(cond) NA_character_
  )[1]
  if (is.na(text)) {
    # Nothing could be made of the declared encoding, so the printable bytes
    # are kept: what is there can still be read, which is better than nothing
    # at all from a field whose encoding its writer got wrong.
    v <- as.integer(bytes)
    v[v < 32L] <- 32L
    text <- iconv(rawToChar(as.raw(v)), "latin1", "UTF-8", sub="?")
  }
  # Whatever happened above, what is handed back is valid UTF-8: a string
  # marked as UTF-8 that is not makes every later call on it fail, so one bad
  # field would otherwise break everything that touched the result.
  text <- iconv(enc2utf8(text), "UTF-8", "UTF-8", sub="?")
  if (is.na(text)) {
    return("")
  }
  return(trimws(text))
}

#' Read a run of bytes as text
#'
#' rawToChar() stops at an embedded nul, and an R string cannot hold one at
#' all, so the nuls are dealt with as bytes before anything is decoded.
#'
#' Where text declared as ISO-8859-1 is in fact valid UTF-8 it is read as
#' UTF-8. xeno-canto declares the comment frame of its recordings as
#' ISO-8859-1 and writes UTF-8 into it, so a copyright sign read as declared
#' comes back as the two characters its bytes spell in ISO-8859-1.
#'
#' @param bytes A raw vector
#' @param from The encoding the bytes are declared to be in
#' @return A string, which is "" where there was nothing to read
#' @keywords internal
#' @noRd
.mdDecode <- function(bytes, from="latin1") {
  bytes <- .mdStripNuls(bytes, from)
  if (length(bytes) == 0L) {
    return("")
  }
  if (identical(from, "latin1") && .mdIsUtf8(bytes)) {
    from <- "UTF-8"
  }
  return(.mdIconv(bytes, from))
}

#' Split a run of bytes on the nuls that separate several values
#' @keywords internal
#' @noRd
.mdSplitNuls <- function(bytes, from="latin1") {
  if (length(bytes) == 0L) {
    return(list())
  }
  if (.mdIsUtf16(from)) {
    bytes <- .mdStripNuls(bytes, from)
    if (length(bytes) < 2L) {
      return(list())
    }
    # A nul unit falls on an even boundary from the start of the text, so a nul
    # byte at an odd offset is half of an ordinary character and not a
    # separator at all.
    units <- seq(1L, length(bytes) - 1L, by=2L)
    at <- units[as.integer(bytes[units]) == 0L & as.integer(bytes[units + 1L]) == 0L]
    starts <- c(1L, at + 2L)
    ends <- c(at - 1L, length(bytes))
  } else {
    at <- which(as.integer(bytes) == 0L)
    if (length(at) == 0L) {
      return(list(bytes))
    }
    starts <- c(1L, at + 1L)
    ends <- c(at - 1L, length(bytes))
  }
  keep <- starts <= ends
  if (!any(keep)) {
    return(list())
  }
  return(mapply(
    function(s, e) bytes[s:e], starts[keep], ends[keep],
    SIMPLIFY=FALSE, USE.NAMES=FALSE
  ))
}

#' Read a run of bytes as the one or more values it holds
#'
#' ID3v2.4 allows a text frame to carry several values, written one after
#' another with a nul between them, which is how a recording credited to two
#' recordists is written.
#'
#' @param bytes A raw vector
#' @param from The encoding the bytes are declared to be in
#' @return A character vector, empty where there was nothing to read
#' @keywords internal
#' @noRd
.mdDecodeValues <- function(bytes, from="latin1") {
  parts <- .mdSplitNuls(bytes, from)
  if (length(parts) == 0L) {
    return(character(0))
  }
  values <- vapply(parts, .mdDecode, character(1), from=from, USE.NAMES=FALSE)
  return(values[nzchar(values)])
}

#' Name a field so that it does not overwrite one already read
#'
#' A file may hold two frames, or two fields, of the same name, which a named
#' list would quietly reduce to the first of them.
#'
#' @param fields The fields read so far
#' @param name The name wanted
#' @return A name that `fields` does not already use
#' @keywords internal
#' @noRd
.mdUniqueName <- function(fields, name) {
  if (!name %in% names(fields)) {
    return(name)
  }
  n <- 2L
  while (paste0(name, ".", n) %in% names(fields)) {
    n <- n + 1L
  }
  return(paste0(name, ".", n))
}

#' Build a metadata object from the fields that were read
#'
#' Every reader gives back a named list of the fields it found, classed by the
#' format it read, so that printing it shows the fields as the file holds them
#' and a caller can tell what it has been handed. A format a file does not
#' carry gives an object holding no fields rather than an error.
#'
#' @param fields A named list of fields
#' @param class The class to give the object
#' @return The fields, classed
#' @keywords internal
#' @noRd
.mdObject <- function(fields, class) {
  if (is.null(fields)) {
    fields <- list()
  }
  return(structure(fields, class=c(class, "audioMetadataFields")))
}

#' Write one field of metadata as a line of text
#' @keywords internal
#' @noRd
.mdFieldText <- function(value) {
  if (is.raw(value)) {
    return(paste0("<", length(value), " bytes>"))
  }
  if (inherits(value, "POSIXt")) {
    return(format(value, "%Y-%m-%dT%H:%M:%S%z"))
  }
  if (length(value) == 0L) {
    return("")
  }
  text <- as.character(value)
  if (!is.null(names(value)) && any(nzchar(names(value)))) {
    text <- paste0(names(value), "=", text)
  }
  return(paste(text, collapse=", "))
}

#' @export
format.audioMetadataFields <- function(x, ...) {
  fields <- unclass(x)
  if (length(fields) == 0L) {
    return(character(0))
  }
  values <- vapply(fields, .mdFieldText, character(1), USE.NAMES=FALSE)
  return(paste0(names(fields), ": ", values))
}

#' @export
print.audioMetadataFields <- function(x, ...) {
  fields <- unclass(x)
  name <- .MD_FORMAT_NAMES[[class(x)[1]]]
  if (is.null(name)) {
    name <- class(x)[1]
  }
  if (length(fields) == 0L) {
    cat("<no ", name, " metadata>\n", sep="")
    return(invisible(x))
  }
  width <- max(nchar(names(fields)))
  for (key in names(fields)) {
    cat(formatC(key, width=width, flag="-"), " ", .mdFieldText(fields[[key]]), "\n", sep="")
  }
  return(invisible(x))
}

#' @export
`[.audioMetadataFields` <- function(x, ...) {
  return(structure(NextMethod(), class=class(x)))
}

#' The one place a wave keeps the metadata its file carried
#'
#' A TaggedWave has a metadata slot made for this, so that is where its
#' metadata goes. A plain Wave has nowhere at all, so an attribute is used
#' instead. Either way there is one store, and every reader goes through these
#' two functions to reach it rather than keeping a place of its own.
#'
#' The slot is typed as a list and an S4 object refuses a classed list in it,
#' so the reading sits under a name of its own inside that list, beside
#' whatever else the caller has chosen to record there.
#'
#' @param x A Wave, WaveMC, TaggedWave or TaggedWaveMC object
#' @return The audioMetadata object the wave carries, or NULL
#' @keywords internal
#' @noRd
.mdStore <- function(x) {
  held <- if (.mdIsTagged(x)) x@metadata[[.MD_STORE]] else attr(x, .MD_STORE)
  if (inherits(held, "audioMetadata")) {
    return(held)
  }
  return(NULL)
}

#' Put metadata in the one place a wave keeps it
#'
#' @param x A Wave, WaveMC, TaggedWave or TaggedWaveMC object
#' @param value An audioMetadata object, or NULL to remove what is there
#' @return The wave, carrying the metadata
#' @keywords internal
#' @noRd
.mdSetStore <- function(x, value) {
  if (.mdIsTagged(x)) {
    # Assigning NULL to a list element removes it, which is what is wanted
    # here: the slot goes back to holding only what the caller put there.
    x@metadata[[.MD_STORE]] <- value
    return(x)
  }
  attr(x, .MD_STORE) <- value
  return(x)
}

#' Is this a wave with a metadata slot of its own?
#' @keywords internal
#' @noRd
.mdIsTagged <- function(x) {
  return(methods::is(x, "TaggedWave") || methods::is(x, "TaggedWaveMC"))
}

#' Read one format of metadata out of the store a wave carries
#'
#' @param x A Wave or WaveMC object
#' @param name The name of the format wanted
#' @return The metadata, or NULL where the object carries none of that format
#' @keywords internal
#' @noRd
.mdFromWave <- function(x, name) {
  held <- .mdStore(x)
  if (is.null(held)) {
    return(NULL)
  }
  return(held$sources[[name]])
}

#' Give one reader its bytes, wherever they are to come from
#'
#' Every reader takes the same things: metadata it has already read, a Wave
#' object carrying it, the raw bytes of a file, or the path to one. Only the
#' last two differ between formats, as reading from a path reads no more of the
#' file than the metadata needs.
#'
#' @param x What the reader was given
#' @param name The name of the format, which is also its class and the
#'   attribute a Wave object holds it under
#' @param fromFile A function of a path
#' @param fromBytes A function of a raw vector
#' @return The metadata, as the given functions build it
#' @keywords internal
#' @noRd
.mdDispatch <- function(x, name, fromFile, fromBytes) {
  if (inherits(x, name)) {
    return(x)
  }
  if (inherits(x, "Wave") || inherits(x, "WaveMC")) {
    held <- .mdFromWave(x, name)
    return(if (is.null(held)) .mdObject(list(), name) else held)
  }
  if (is.raw(x)) {
    return(fromBytes(x))
  }
  .mdCheckFile(x)
  return(fromFile(x))
}

#' Check that a path names a file that can be read
#'
#' A directory passes file.exists(), and opening one fails with a message about
#' permissions that says nothing about what was actually wrong.
#'
#' @param x The path given
#' @return Nothing, called for the error it raises
#' @keywords internal
#' @noRd
.mdCheckFile <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("x must be a file path, a Wave object, or a raw vector.")
  }
  if (dir.exists(x)) {
    stop("Path is a directory, not a file: ", x)
  }
  if (!file.exists(x)) {
    stop("File does not exist: ", x)
  }
  return(invisible(NULL))
}

#' Read a run of bytes from a file by position
#'
#' @param file Path to the file
#' @param at The one-based position of the first byte wanted
#' @param n The number of bytes to read
#' @return A raw vector, holding however many of those bytes the file has
#' @keywords internal
#' @noRd
.mdFileRange <- function(file, at, n) {
  if (n <= 0) {
    return(raw(0))
  }
  con <- file(file, "rb")
  on.exit(close(con))
  return(.wavConReader(con)(at, n))
}

#' Read the start of a file
#' @keywords internal
#' @noRd
.mdFileHead <- function(file, n=.MD_HEAD_READ) {
  return(.mdFileRange(file, 1L, n))
}

#' Name the container a file is held in
#'
#' Read from the bytes of the file rather than from its name. The corpora this
#' was written against hold MP3, Ogg and zero-crossing files under a .wav
#' extension, and a reader that went by the name would read none of them.
#'
#' @param bytes The first bytes of the file
#' @return A container name, or NA where it is not one that is recognised
#' @keywords internal
#' @noRd
.mdContainer <- function(bytes) {
  if (length(bytes) < 12L) {
    return(NA_character_)
  }
  first <- .rawToId(bytes[1:4])
  if (first == "RIFF") {
    form <- .rawToId(bytes[9:12])
    return(if (form == "WAVE") "WAVE" else paste0("RIFF/", form))
  }
  if (first == "RF64") {
    return("RF64")
  }
  if (first == "fLaC") {
    return("FLAC")
  }
  if (first == "OggS") {
    return("Ogg")
  }
  if (first == "FORM") {
    form <- .rawToId(bytes[9:12])
    return(if (form == "AIFF") "AIFF" else paste0("AIFF/", form))
  }
  if (.rawToId(bytes[5:8]) == "ftyp") {
    return("MP4")
  }
  if (substr(first, 1L, 3L) == "ID3") {
    return("MP3")
  }
  # An MP3 with no tag at all starts at a frame header, which is eleven set
  # bits. This is the last thing tried: those bytes say little on their own,
  # and anything that identifies itself has been identified by now.
  if (as.integer(bytes[1]) == 255L && bitwAnd(as.integer(bytes[2]), 224L) == 224L) {
    return("MP3")
  }
  return(NA_character_)
}
