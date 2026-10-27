# ID3 metadata, versions 1 and 2.2 to 2.4.
#
# ID3 is the metadata format of MP3 files, and is also found inside a wave file
# as an "id3 " chunk. A version 2 tag sits at the start of the file and holds a
# run of frames, each named by four characters, or by three in version 2.2; a
# version 1 tag is 128 bytes of fixed-width fields at the very end.
#
# The sound libraries this package was written against put the whole of their
# record for a recording into a version 2.4 tag: the species in TIT2, the
# family in TCON, the recordist in TPE1, and the locality, coordinates,
# elevation, time, call type and remarks in COMM.

#' Bytes of a version 2 tag header
#' @keywords internal
#' @noRd
.ID3_HEADER <- 10L

#' Read the encoding a frame declares for its text
#'
#' @param code The encoding byte of the frame
#' @param bytes The text the frame holds, needed to look for a byte order mark
#' @return An encoding name
#' @keywords internal
#' @noRd
.id3Encoding <- function(code, bytes) {
  if (code == 1L) {
    # UTF-16 as declared by version 2.2 and 2.3 carries a byte order mark,
    # which says which of the two orders it is in and is not part of the text.
    if (length(bytes) >= 2L) {
      lead <- as.integer(bytes[1:2])
      if (identical(lead, c(255L, 254L)) || identical(lead, c(254L, 255L))) {
        return("UTF-16")
      }
    }
    return("UTF-16BE")
  }
  if (code == 2L) {
    return("UTF-16BE")
  }
  if (code == 3L) {
    return("UTF-8")
  }
  return("latin1")
}

#' Undo the unsynchronisation of a run of bytes
#'
#' A tag may be written so that no run of bytes in it can be mistaken for the
#' start of an MP3 frame, by following every 255 with a nul. Those nuls are not
#' part of the tag and are dropped before it is read.
#'
#' @param bytes A raw vector
#' @return The bytes, with the added nuls removed
#' @keywords internal
#' @noRd
.id3Deunsync <- function(bytes) {
  n <- length(bytes)
  if (n < 2L) {
    return(bytes)
  }
  at <- which(as.integer(bytes) == 255L)
  at <- at[at < n]
  if (length(at) == 0L) {
    return(bytes)
  }
  added <- at[as.integer(bytes[at + 1L]) == 0L]
  if (length(added) == 0L) {
    return(bytes)
  }
  return(bytes[-(added + 1L)])
}

#' Split the description of a frame from the value that follows it
#'
#' Several kinds of frame carry a description before their value, ended by a
#' nul, or by a nul character of two bytes where the text is UTF-16.
#'
#' @param bytes The frame, from the start of its description
#' @param encoding The encoding the frame declared
#' @return The description and the bytes that follow it
#' @keywords internal
#' @noRd
.id3SplitDescription <- function(bytes, encoding) {
  nothing <- list(description="", rest=bytes)
  if (length(bytes) < 2L) {
    return(nothing)
  }
  if (.mdIsUtf16(encoding)) {
    units <- seq(1L, length(bytes) - 1L, by=2L)
    at <- units[as.integer(bytes[units]) == 0L & as.integer(bytes[units + 1L]) == 0L]
    if (length(at) == 0L) {
      return(nothing)
    }
    first <- at[1]
    after <- first + 2L
  } else {
    at <- which(as.integer(bytes) == 0L)
    if (length(at) == 0L) {
      return(nothing)
    }
    first <- at[1]
    after <- first + 1L
  }
  return(list(
    description = if (first > 1L) .mdDecode(bytes[1:(first - 1L)], from=encoding) else "",
    rest = if (after <= length(bytes)) bytes[after:length(bytes)] else raw(0)
  ))
}

#' Name a frame that carries a description by its description
#'
#' A file may hold several comments, or several of the user-defined frames, and
#' it is the description that says which is which.
#'
#' @param id The frame identifier
#' @param description The description the frame carried
#' @return A name for the field
#' @keywords internal
#' @noRd
.id3Name <- function(id, description) {
  if (!nzchar(description)) {
    return(id)
  }
  return(paste0(id, "|", description))
}

#' Read a frame that carries a description before its value
#' @keywords internal
#' @noRd
.id3Described <- function(id, bytes, encoding, url=FALSE) {
  split <- .id3SplitDescription(bytes, encoding)
  # The address in a user-defined link frame is written in ISO-8859-1 whatever
  # the frame declared, which applies only to its description.
  value <- if (url) .mdDecode(split$rest) else .mdDecodeValues(split$rest, from=encoding)
  if (length(value) == 0L || !any(nzchar(value))) {
    return(NULL)
  }
  return(list(name=.id3Name(id, split$description), value=value))
}

#' Read the value a frame holds
#'
#' @param id The frame identifier
#' @param data The body of the frame
#' @return The name and value to record, or NULL for a frame whose value this
#'   package does not read
#' @keywords internal
#' @noRd
.id3Value <- function(id, data) {
  if (id %in% c("COMM", "COM", "USLT", "ULT")) {
    # An encoding, a three-letter language, a description and then the text.
    if (length(data) < 5L) {
      return(NULL)
    }
    rest <- data[-1L]
    language <- .mdDecode(rest[1:3])
    rest <- rest[-(1:3)]
    encoding <- .id3Encoding(as.integer(data[1]), rest)
    split <- .id3SplitDescription(rest, encoding)
    value <- .mdDecode(split$rest, from=encoding)
    if (!nzchar(value)) {
      return(NULL)
    }
    return(list(
      name = .id3Name(id, split$description),
      value = value,
      language = language
    ))
  }
  if (grepl("^T", id)) {
    if (length(data) < 2L) {
      return(NULL)
    }
    rest <- data[-1L]
    encoding <- .id3Encoding(as.integer(data[1]), rest)
    if (id %in% c("TXXX", "TXX")) {
      return(.id3Described(id, rest, encoding))
    }
    values <- .mdDecodeValues(rest, from=encoding)
    if (length(values) == 0L) {
      return(NULL)
    }
    return(list(name=id, value=values))
  }
  if (grepl("^W", id)) {
    if (id %in% c("WXXX", "WXX")) {
      if (length(data) < 2L) {
        return(NULL)
      }
      rest <- data[-1L]
      return(.id3Described(id, rest, .id3Encoding(as.integer(data[1]), rest), url=TRUE))
    }
    value <- .mdDecode(data)
    if (!nzchar(value)) {
      return(NULL)
    }
    return(list(name=id, value=value))
  }
  # Artwork, counters, identifiers, synchronised words and everything else a
  # frame can hold are not text, and are passed over rather than read as it.
  return(NULL)
}

#' Strip the extra bytes a frame's flags add, or refuse the frame
#'
#' @param data The body of the frame as the file holds it
#' @param major The major version of the tag
#' @param flags The format flags of the frame
#' @param unsynchronised Whether the tag header said every frame in it is
#'   unsynchronised, which a version 2.4 tag says there rather than, or as well
#'   as, in each frame
#' @return The body of the frame, or NULL for a frame that cannot be read
#' @keywords internal
#' @noRd
.id3FrameData <- function(data, major, flags, unsynchronised=FALSE) {
  if (major == 3L) {
    # Compressed with zlib, or encrypted: in either case the bytes cannot be
    # read without undoing that first.
    if (bitwAnd(flags, 128L) != 0L || bitwAnd(flags, 64L) != 0L) {
      return(NULL)
    }
    if (bitwAnd(flags, 32L) != 0L) {
      data <- data[-1L]
    }
  } else if (major >= 4L) {
    if (bitwAnd(flags, 64L) != 0L) {
      data <- data[-1L]
    }
    if (bitwAnd(flags, 8L) != 0L || bitwAnd(flags, 4L) != 0L) {
      return(NULL)
    }
    if (bitwAnd(flags, 1L) != 0L) {
      if (length(data) <= 4L) {
        return(NULL)
      }
      data <- data[-(1:4)]
    }
    # A tag unsynchronised as a whole has every frame in it unsynchronised, and
    # the specification asks that each frame say so too. A writer that said it
    # only in the tag header is taken at its word, and one that said it in both
    # places has the nuls taken out once rather than twice: a second pass would
    # take out a nul that the text itself holds after a 255.
    if (bitwAnd(flags, 2L) != 0L || unsynchronised) {
      data <- .id3Deunsync(data)
    }
  }
  if (length(data) == 0L) {
    return(NULL)
  }
  return(data)
}

#' Read the frames of a version 2 tag
#'
#' @param body The tag, from the first frame
#' @param major The major version of the tag
#' @param unsynchronised Whether the tag header said every frame in it is
#'   unsynchronised
#' @return The fields read, the frames passed over, and their languages
#' @keywords internal
#' @noRd
.id3Frames <- function(body, major, unsynchronised=FALSE) {
  idLength <- if (major == 2L) 3L else 4L
  headerLength <- if (major == 2L) 6L else .ID3_HEADER
  fields <- list()
  skipped <- numeric(0)
  languages <- character(0)
  pos <- 1L
  while (pos + headerLength - 1L <= length(body)) {
    id <- .rawToId(body[pos:(pos + idLength - 1L)])
    # Past the last frame a tag is padded with nuls, which is where the
    # identifier stops looking like one. A file whose frames have run into one
    # another stops here too, rather than reading the audio as metadata.
    if (!grepl("^[A-Z0-9]+$", id)) {
      break
    }
    size <- if (major == 2L) {
      .rawToUIntBE(body[(pos + 3L):(pos + 5L)])
    } else if (major >= 4L) {
      .rawToSyncsafe(body[(pos + 4L):(pos + 7L)])
    } else {
      .rawToUIntBE(body[(pos + 4L):(pos + 7L)])
    }
    if (size <= 0 || pos + headerLength + size - 1 > length(body)) {
      break
    }
    flags <- if (major == 2L) 0L else as.integer(body[pos + 9L])
    data <- body[(pos + headerLength):(pos + headerLength + size - 1)]
    pos <- pos + headerLength + size

    data <- .id3FrameData(data, major, flags, unsynchronised)
    read <- if (is.null(data)) NULL else .id3Value(id, data)
    if (is.null(read)) {
      skipped[id] <- size
      next
    }
    name <- .mdUniqueName(fields, read$name)
    fields[[name]] <- read$value
    if (!is.null(read$language) && nzchar(read$language)) {
      languages[name] <- read$language
    }
  }
  return(list(fields=fields, skipped=skipped, languages=languages))
}

#' Read a version 2 tag
#'
#' Tells a tag that holds nothing a reader here can use, which is still a tag
#' and is still what the file carries, from bytes that are not a tag at all,
#' where there is somewhere else left to look.
#'
#' @param bytes A raw vector starting at the tag header
#' @return An id3 object, or NULL where the bytes hold no tag this can read
#' @keywords internal
#' @noRd
.id3Parse <- function(bytes) {
  if (length(bytes) < .ID3_HEADER || .rawToId(bytes[1:3]) != "ID3") {
    return(NULL)
  }
  major <- as.integer(bytes[4])
  minor <- as.integer(bytes[5])
  flags <- as.integer(bytes[6])
  size <- .rawToSyncsafe(bytes[7:10])
  if (major < 2L || major > 4L) {
    warning(
      "Ignoring an ID3v2.", major, " tag: this package reads versions 2.2 to 2.4."
    )
    return(NULL)
  }
  # Version 2.2 used the bit that later versions use for an extended header to
  # say that the whole tag is compressed, and told a reader that met it to
  # ignore the tag, as no scheme for the compression was ever specified.
  if (major == 2L && bitwAnd(flags, 64L) != 0L) {
    warning("Ignoring a compressed ID3v2.2 tag.")
    return(NULL)
  }
  last <- min(length(bytes), .ID3_HEADER + size)
  if (last <= .ID3_HEADER) {
    return(NULL)
  }
  body <- bytes[(.ID3_HEADER + 1L):last]
  # Version 2.3 unsynchronises the tag as one run of bytes and writes its frame
  # sizes to describe that run with the added nuls gone, so there the whole tag
  # is put back before the frames are walked.
  #
  # Version 2.4 unsynchronises each frame on its own and sizes every frame as
  # it is stored. Putting the whole tag back first would shorten the body while
  # its sizes still described the longer form, so the walk would lose its place
  # at the first byte that had been unsynchronised: it would read the start of
  # the next frame as the end of this one's text and stop at the frame after
  # that, having silently dropped the rest of the tag. There it is left to each
  # frame, which knows its own stored length.
  unsynchronised <- bitwAnd(flags, 128L) != 0L
  if (unsynchronised && major < 4L) {
    body <- .id3Deunsync(body)
  }
  at <- 1L
  if (major >= 3L && bitwAnd(flags, 64L) != 0L && length(body) >= 4L) {
    # Version 2.3 gives the size of the extended header without counting the
    # four bytes that hold it; version 2.4 counts them, and writes it
    # sync-safe as it writes every other size.
    at <- at + if (major >= 4L) .rawToSyncsafe(body[1:4]) else 4L + .rawToUIntBE(body[1:4])
  }
  if (at > length(body)) {
    return(NULL)
  }
  read <- .id3Frames(body[at:length(body)], major, unsynchronised && major >= 4L)
  return(structure(
    .mdObject(read$fields, "id3"),
    version = paste0("2.", major, ".", minor),
    skipped = read$skipped,
    languages = read$languages
  ))
}

#' Read a version 1 tag
#'
#' @param bytes A raw vector holding the end of a file, or the whole of one
#' @return An id3 object
#' @keywords internal
#' @noRd
.id3v1 <- function(bytes) {
  n <- length(bytes)
  if (n < 128L) {
    return(.mdObject(list(), "id3"))
  }
  tag <- bytes[(n - 127L):n]
  if (.rawToId(tag[1:3]) != "TAG") {
    return(.mdObject(list(), "id3"))
  }
  # Version 1.1 took two bytes off the comment to make room for a track
  # number, and marks that it has done so by leaving the first of them a nul.
  numbered <- as.integer(tag[126]) == 0L && as.integer(tag[127]) != 0L
  fields <- list(
    title = .mdDecode(tag[4:33]),
    artist = .mdDecode(tag[34:63]),
    album = .mdDecode(tag[64:93]),
    year = .mdDecode(tag[94:97]),
    comment = .mdDecode(if (numbered) tag[98:125] else tag[98:127])
  )
  fields <- fields[nzchar(unlist(fields))]
  # "TAG" followed by a run of nuls is what a tagger writes when it has
  # nothing to say. Reading a genre of 0, which the format defines as a real
  # genre, out of those nuls would make such a file out to carry a record it
  # does not have.
  if (length(fields) == 0L) {
    return(.mdObject(list(), "id3"))
  }
  if (numbered) {
    fields$track <- as.integer(tag[127])
  }
  # 255 is how a writer says it recorded no genre at all.
  genre <- as.integer(tag[128])
  if (genre != 255L) {
    fields$genre <- genre
  }
  return(structure(
    .mdObject(fields, "id3"),
    version = if (numbered) "1.1" else "1.0"
  ))
}

#' Read the ID3 tag of a file
#' @keywords internal
#' @noRd
.id3FromFile <- function(file) {
  head <- .mdFileHead(file, 16L)
  container <- .mdContainer(head)
  if (length(head) >= .ID3_HEADER && .rawToId(head[1:3]) == "ID3") {
    size <- .rawToSyncsafe(head[7:10])
    tag <- .id3Parse(.mdFileRange(file, 1L, min(.ID3_HEADER + size, .MD_BLOCK_MAX)))
    if (!is.null(tag)) {
      return(tag)
    }
  }
  # A wave file carries a whole version 2 tag inside a chunk of its own.
  if (.mdWaveContainer(container)) {
    for (id in c("id3 ", "ID3 ")) {
      body <- .mdWavChunk(file, id)
      if (!is.null(body)) {
        tag <- .id3Parse(body)
        if (!is.null(tag)) {
          return(tag)
        }
      }
    }
  }
  if (!.id3v1Possible(container)) {
    return(.mdObject(list(), "id3"))
  }
  size <- file.size(file)
  if (is.na(size) || size < 128) {
    return(.mdObject(list(), "id3"))
  }
  return(.id3v1(.mdFileRange(file, size - 127, 128L)))
}

#' Could the end of this file hold a version 1 tag?
#'
#' A version 1 tag is 128 bytes at the very end of the file, marked by nothing
#' more than the three letters "TAG", which the audio of a wave file can spell
#' by chance. A format that has somewhere of its own to keep metadata is
#' therefore not searched for one: appending a version 1 tag to anything but an
#' MP3 is not what any of these formats say to do, and reading three bytes of
#' audio as a tag would invent a record the file does not carry.
#'
#' @param container The container the file was found to be, which is NA where
#'   it is not one this package recognises
#' @return A logical value
#' @keywords internal
#' @noRd
.id3v1Possible <- function(container) {
  return(is.na(container) || identical(container, "MP3"))
}

#' Read the ID3 tag held in raw bytes
#' @keywords internal
#' @noRd
.id3FromBytes <- function(bytes) {
  if (length(bytes) >= .ID3_HEADER && .rawToId(bytes[1:3]) == "ID3") {
    tag <- .id3Parse(bytes)
    if (!is.null(tag)) {
      return(tag)
    }
  }
  for (id in c("id3 ", "ID3 ")) {
    body <- .mdWavChunkBytes(bytes, id)
    if (!is.null(body)) {
      tag <- .id3Parse(body)
      if (!is.null(tag)) {
        return(tag)
      }
    }
  }
  if (!.id3v1Possible(.mdContainer(bytes))) {
    return(.mdObject(list(), "id3"))
  }
  return(.id3v1(bytes))
}

#' Read ID3 metadata
#'
#' Reads the ID3 tag of an MP3 file, or the one a wave file holds in an "id3 "
#' chunk. Versions 2.2, 2.3 and 2.4 are read, as is the older version 1 tag at
#' the end of a file where there is no version 2 tag to read instead. A file
#' holding no tag gives an empty result rather than an error.
#'
#' @details
#' Fields are named by the identifiers the frames themselves carry, such as
#' `TIT2` for a title, `TPE1` for an artist and `COMM` for a comment, rather
#' than being renamed, so that a frame this package has never heard of reads as
#' readily as the ones it has. A frame that carries a description, which is any
#' comment or user-defined frame, is named by it as well, as in
#' `TXXX|Recordist`; a name that would otherwise repeat gets a number, as in
#' `COMM.2`. A version 1 tag has no frames and its fields are named `title`,
#' `artist`, `album`, `year`, `comment`, `track` and `genre`, the last being
#' the numeric code the format defines rather than its name.
#'
#' The version of the tag is kept on the result as its "version" attribute. The
#' languages of any frames that declared one are kept as "languages", and the
#' sizes of the frames whose values this package does not read, which is
#' artwork, counters, identifiers and anything else that is not text, as
#' "skipped".
#'
#' Text declared as ISO-8859-1 that is in fact valid UTF-8 is read as UTF-8,
#' which is what xeno-canto writes into the comment frame of its recordings.
#'
#' A version 1 tag is marked by nothing more than the three letters "TAG" at a
#' fixed distance from the end of the file, which the audio of a wave file can
#' spell by chance, so one is only looked for in an MP3 file or in a file of no
#' format this package recognises. A wave file keeps its tag in a chunk, where
#' there is no guessing involved.
#'
#' @param x Path to an audio file, a Wave object with ID3 metadata attached, a
#'   raw vector holding a file, or an id3 object
#' @return An id3 object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once.
#' @references
#'   Nilsson, M. (2000). ID3 tag version 2.4.0.
#'   \url{https://id3.org/id3v2.4.0-structure}
#' @examples
#' \dontrun{
#' m <- id3("XC428067.mp3")
#' m[["TIT2"]]
#' attr(m, "version")
#' }
#'
id3 <- function(x) {
  return(.mdDispatch(x, "id3", fromFile=.id3FromFile, fromBytes=.id3FromBytes))
}
