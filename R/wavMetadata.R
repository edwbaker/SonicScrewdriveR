# The metadata chunks a wave file can carry, other than GUANO, which has its
# own reader in guano.R.
#
# A wave file is a RIFF file: a sequence of chunks, each named by four
# characters. Several conventions put metadata in one, and a single file may
# hold more than one of them, written by whatever passed the recording along.

#' Read the body of one metadata chunk of a wave file
#'
#' Only the chunk itself is read, rather than the whole recording, and no more
#' of it than metadata could reasonably be. A file damaged in the right place
#' can declare a chunk larger than any machine could hold.
#'
#' @param file Path to a wave file, or a map of one already walked
#' @param id The chunk identifier to look for
#' @param cap Most bytes to read
#' @return A raw vector, or NULL where the file has no such chunk, holds
#'   nothing in it, or is not a wave file at all
#' @keywords internal
#' @noRd
.mdWavChunk <- function(file, id, cap=.MD_BLOCK_MAX) {
  if (inherits(file, "wavChunkMap")) {
    return(.mdMapChunk(file, id, cap))
  }
  found <- tryCatch({
    con <- file(file, "rb")
    on.exit(close(con))
    size <- file.size(file)
    read <- .wavConReader(con)
    chunk <- .wavFindChunk(.wavWalk(read, size, size)$chunks, id)
    if (is.null(chunk) || chunk$size < 1) {
      NULL
    } else {
      read(chunk$start, min(chunk$size, cap))
    }
  }, unsupportedWav=function(cond) NULL)
  return(found)
}

#' Read the body of one metadata chunk of a wave file held as raw bytes
#'
#' @param bytes A raw vector holding a wave file, or the start of one
#' @param id The chunk identifier to look for
#' @param cap Most bytes to read
#' @return A raw vector, or NULL where the bytes hold no such chunk
#' @keywords internal
#' @noRd
.mdWavChunkBytes <- function(bytes, id, cap=.MD_BLOCK_MAX) {
  info <- tryCatch(.wavChunks(bytes), unsupportedWav=function(cond) NULL)
  if (is.null(info)) {
    return(NULL)
  }
  chunk <- .wavFindChunk(info$chunks, id)
  if (is.null(chunk) || chunk$size < 1) {
    return(NULL)
  }
  n <- min(chunk$size, cap, length(bytes) - chunk$start + 1)
  if (n < 1) {
    return(NULL)
  }
  return(bytes[chunk$start:(chunk$start + n - 1)])
}

#' Read every chunk of a wave file carrying a given identifier
#'
#' A wave file may hold more than one list: one of cue labels and one of
#' information, in either order. Taking the first and looking no further would
#' miss whatever is behind it.
#'
#' @param file Path to a wave file, or a map of one already walked
#' @param id The chunk identifier to look for
#' @param cap Most bytes to read from each chunk
#' @return A list of raw vectors, empty where the file has no such chunk
#' @keywords internal
#' @noRd
.mdWavChunks <- function(file, id, cap=.MD_BLOCK_MAX) {
  if (inherits(file, "wavChunkMap")) {
    return(.mdMapChunks(file, id, cap))
  }
  found <- tryCatch({
    con <- file(file, "rb")
    on.exit(close(con))
    size <- file.size(file)
    read <- .wavConReader(con)
    bodies <- list()
    for (chunk in .wavWalk(read, size, size)$chunks) {
      if (chunk$id == id && chunk$size >= 1) {
        bodies[[length(bodies) + 1L]] <- read(chunk$start, min(chunk$size, cap))
      }
    }
    bodies
  }, unsupportedWav=function(cond) list())
  return(found)
}

#' Read every chunk of a given identifier from a wave file held as raw bytes
#' @keywords internal
#' @noRd
.mdWavChunksBytes <- function(bytes, id, cap=.MD_BLOCK_MAX) {
  info <- tryCatch(.wavChunks(bytes), unsupportedWav=function(cond) NULL)
  if (is.null(info)) {
    return(list())
  }
  bodies <- list()
  for (chunk in info$chunks) {
    if (chunk$id != id || chunk$size < 1) {
      next
    }
    n <- min(chunk$size, cap, length(bytes) - chunk$start + 1)
    if (n >= 1) {
      bodies[[length(bodies) + 1L]] <- bytes[chunk$start:(chunk$start + n - 1)]
    }
  }
  return(bodies)
}

#' The identifiers of the chunks a wave file holds
#'
#' @param x Path to a wave file, a raw vector holding one, or a map of one
#'   already walked
#' @return A character vector of chunk identifiers, in the order the file holds
#'   them, empty where it is not a wave file
#' @keywords internal
#' @noRd
.mdWavChunkIds <- function(x) {
  if (inherits(x, "wavChunkMap")) {
    return(vapply(x$chunks, function(chunk) chunk$id, character(1)))
  }
  info <- tryCatch(
    if (is.raw(x)) .wavChunks(x) else .wavChunksFile(x),
    unsupportedWav=function(cond) NULL,
    error=function(cond) NULL
  )
  if (is.null(info)) {
    return(character(0))
  }
  return(vapply(info$chunks, function(chunk) chunk$id, character(1)))
}

# ----------------------------------------------------------------- LIST INFO --

#' Bytes of a field of an associated data list that come before its text
#'
#' The fields of an INFO list hold nothing but text. Those of an associated
#' data list, which is what carries the labels of cue points, begin with the
#' identifier of the cue point they belong to, and the labelled text field
#' begins with a whole header of sample counts, purposes and language codes
#' besides. Reading those bytes as text gives a run of nuls and mojibake in
#' front of the label.
#'
#' @param kind The kind of list the field is in
#' @param id The identifier of the field
#' @return The number of bytes to pass over
#' @keywords internal
#' @noRd
.infoPrefix <- function(kind, id) {
  if (kind != "adtl") {
    return(0L)
  }
  # A cue point identifier.
  if (id %in% c("labl", "note")) {
    return(4L)
  }
  # A cue point identifier, a sample count, a purpose, and the country,
  # language, dialect and code page the text is written for.
  if (id == "ltxt") {
    return(20L)
  }
  return(0L)
}

#' Read the fields of one RIFF list
#'
#' A list holds its kind in its first four bytes and then a run of fields, each
#' an identifier, a size and that many bytes of body.
#'
#' @param body The body of a LIST chunk
#' @param fields The fields read from earlier lists
#' @return The fields, with those of this list added
#' @keywords internal
#' @noRd
.infoFields <- function(body, fields=list()) {
  if (length(body) < 4L) {
    return(fields)
  }
  kind <- .rawToId(body[1:4])
  pos <- 5L
  while (pos + 7L <= length(body)) {
    id <- .rawToId(body[pos:(pos + 3L)])
    size <- .rawToUInt(body[(pos + 4L):(pos + 7L)])
    if (size < 0 || pos + 8L + size - 1 > length(body)) {
      break
    }
    skip <- .infoPrefix(kind, id)
    value <- if (size > skip) {
      .mdDecode(body[(pos + 8L + skip):(pos + 8L + size - 1)])
    } else {
      ""
    }
    if (nzchar(value)) {
      # A list of information is the usual one and its fields are named by
      # their own identifiers. Anything else is named by its list as well, so
      # that a cue label is not mistaken for something said about the
      # recording as a whole.
      name <- if (kind == "INFO") id else paste0(kind, "/", id)
      fields[[.mdUniqueName(fields, name)]] <- value
    }
    # The fields of a list are padded to an even number of bytes, as chunks are.
    pos <- pos + 8L + size + (size %% 2)
  }
  return(fields)
}

#' Read the INFO metadata of a wave file
#'
#' Reads the fields of the RIFF lists of a wave file. The list of information
#' is where a recorder, an editor or an archive writes what it has to say about
#' a recording in no particular format: `ICMT` holds a comment, which is where
#' AudioMoth firmware records the settings a recording was made with, `INAM` a
#' name, `IART` an artist and `ICRD` a date. A file holding no list gives an
#' empty result rather than an error.
#'
#' Fields of the list of information are named by their own four-character
#' identifiers. A file may hold other lists, most often `adtl`, which carries
#' the labels of cue points rather than anything about the recording as a
#' whole; the fields of those are named by their list as well, as in
#' `adtl/labl`.
#'
#' @param x Path to a wave file, a Wave object with INFO metadata attached, a
#'   raw vector holding a wave file, or a wavInfo object
#' @return A wavInfo object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once, and [guano()], [bext()], [wamd()] and [ixml()] read the
#'   other formats on their own.
#' @examples
#' f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
#' wavInfo(f)
#'
wavInfo <- function(x) {
  return(.mdDispatch(
    x, "wavInfo",
    fromFile=function(file) .mdInfoObject(.mdWavChunks(file, "LIST")),
    fromBytes=function(bytes) .mdInfoObject(.mdWavChunksBytes(bytes, "LIST"))
  ))
}

#' Build a wavInfo object from the bodies of the lists a file holds
#' @keywords internal
#' @noRd
.mdInfoObject <- function(bodies) {
  fields <- list()
  for (body in bodies) {
    fields <- .infoFields(body, fields)
  }
  return(.mdObject(fields, "wavInfo"))
}

# --------------------------------------------------------------------- iXML --

#' Undo the escaping of XML text
#'
#' iXML is written as XML, where the five characters that mark up a document
#' are escaped wherever they appear in a value, and any character at all may
#' be written as a numeric reference.
#'
#' @param text A string
#' @return The string, with its references resolved
#' @keywords internal
#' @noRd
.mdUnescapeXml <- function(text) {
  numeric <- function(matched, base, prefix) {
    code <- strtoi(sub(prefix, "", sub(";$", "", matched)), base=base)
    if (is.na(code) || code < 1 || code > 1114111) {
      return(matched)
    }
    return(intToUtf8(code))
  }
  for (pattern in c("&#x[0-9A-Fa-f]+;", "&#[0-9]+;")) {
    found <- regmatches(text, gregexpr(pattern, text))[[1]]
    for (one in unique(found)) {
      hex <- grepl("^&#x", one)
      text <- gsub(
        one,
        numeric(one, if (hex) 16L else 10L, if (hex) "^&#x" else "^&#"),
        text,
        fixed=TRUE
      )
    }
  }
  # The ampersand goes last: undoing it first would turn "&amp;lt;" into a
  # less-than sign, which is not what the document said.
  text <- gsub("&lt;", "<", text, fixed=TRUE)
  text <- gsub("&gt;", ">", text, fixed=TRUE)
  text <- gsub("&quot;", "\"", text, fixed=TRUE)
  text <- gsub("&apos;", "'", text, fixed=TRUE)
  text <- gsub("&amp;", "&", text, fixed=TRUE)
  return(text)
}

#' Read the iXML metadata of a wave file
#'
#' Reads the iXML chunk of a wave file, the convention by which field
#' recorders and the software that handles their files describe a take:
#' `PROJECT`, `SCENE`, `TAKE`, `TAPE` and `NOTE`, among many others that
#' particular recorders add.
#'
#' Every element of the document that holds text rather than further elements
#' is read, named by its own tag, so that fields this package has never heard
#' of are read as readily as the ones it has. A tag used more than once, as the
#' entries of a track list use theirs, gives `TAG`, `TAG.2` and so on. The
#' document itself is kept on the result as its "xml" attribute.
#'
#' @param x Path to a wave file, a Wave object with iXML metadata attached, a
#'   raw vector holding a wave file, or an ixml object
#' @return An ixml object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once.
#' @examples
#' \dontrun{
#' ixml("recording.wav")
#' }
#'
ixml <- function(x) {
  return(.mdDispatch(
    x, "ixml",
    fromFile=function(file) .mdIxmlObject(.mdWavChunk(file, "iXML")),
    fromBytes=function(bytes) .mdIxmlObject(.mdWavChunkBytes(bytes, "iXML"))
  ))
}

#' Build an ixml object from the body of an iXML chunk
#' @keywords internal
#' @noRd
.mdIxmlObject <- function(body) {
  if (is.null(body) || length(body) == 0L) {
    return(.mdObject(list(), "ixml"))
  }
  text <- .mdDecode(body, from="UTF-8")
  if (!nzchar(text)) {
    return(.mdObject(list(), "ixml"))
  }
  fields <- list()
  # An element holding no other element holds a value. One holding elements is
  # a container, and is passed over so that its own leaves are read instead of
  # the run of text they would spell together.
  found <- regmatches(
    text,
    gregexpr(
      "<([A-Za-z_][A-Za-z0-9_.:-]*)(?:[[:space:]][^<>]*)?>[^<>]*</\\1>",
      text,
      perl=TRUE
    )
  )[[1]]
  for (one in found) {
    tag <- sub("^<([A-Za-z_][A-Za-z0-9_.:-]*).*$", "\\1", one)
    value <- trimws(.mdUnescapeXml(sub("^<[^>]*>(.*)</[^>]*>$", "\\1", one)))
    if (nzchar(value)) {
      fields[[.mdUniqueName(fields, tag)]] <- value
    }
  }
  return(structure(.mdObject(fields, "ixml"), xml=text))
}
