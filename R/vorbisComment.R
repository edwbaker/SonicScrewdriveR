# Vorbis comments, the metadata of FLAC and Ogg files.
#
# A Vorbis comment block is a vendor string and then a run of "KEY=value"
# strings, each preceded by its length as four little-endian bytes. FLAC holds
# one in a metadata block of its own; an Ogg file holds it in the second packet
# of its stream, after a signature that says what kind of stream it is.

#' Most comments read from one block
#'
#' A block declares how many comments it holds, and a damaged one can declare
#' more than any file could hold.
#' @keywords internal
#' @noRd
.MD_VORBIS_MAX <- 1000L

#' Most FLAC metadata blocks walked before giving up
#' @keywords internal
#' @noRd
.MD_FLAC_BLOCKS <- 128L

#' Fetch a run of bytes from a raw vector by position
#'
#' Returns however many of the bytes asked for are there, as reading from a
#' connection does, so that the same walk can read from either.
#'
#' @param bytes A raw vector
#' @return A function of a one-based position and a number of bytes
#' @keywords internal
#' @noRd
.mdBytesReader <- function(bytes) {
  function(at, n) {
    if (at < 1L || at > length(bytes) || n < 1L) {
      return(raw(0))
    }
    return(bytes[at:min(at + n - 1L, length(bytes))])
  }
}

#' Find where a run of bytes appears
#'
#' @param bytes A raw vector to search
#' @param pattern A raw vector to look for
#' @return The one-based positions at which the pattern starts
#' @keywords internal
#' @noRd
.mdFindBytes <- function(bytes, pattern) {
  n <- length(pattern)
  if (n == 0L || length(bytes) < n) {
    return(integer(0))
  }
  at <- which(bytes == pattern[1])
  at <- at[at + n - 1L <= length(bytes)]
  if (length(at) == 0L) {
    return(integer(0))
  }
  found <- vapply(
    at,
    function(start) identical(bytes[start:(start + n - 1L)], pattern),
    logical(1)
  )
  return(at[found])
}

#' Read the fields of a Vorbis comment block
#'
#' @param bytes A raw vector holding the block, and possibly more besides
#' @param at The one-based position the block starts at
#' @return A named list of fields
#' @keywords internal
#' @noRd
.vorbisFields <- function(bytes, at=1L) {
  fields <- list()
  uint <- function(pos) {
    if (pos < 1L || pos + 3L > length(bytes)) {
      return(NA_real_)
    }
    return(.rawToUInt(bytes[pos:(pos + 3L)]))
  }

  pos <- at
  size <- uint(pos)
  if (is.na(size) || size < 0 || pos + 4L + size - 1 > length(bytes)) {
    return(fields)
  }
  pos <- pos + 4L
  if (size > 0) {
    vendor <- .mdDecode(bytes[pos:(pos + size - 1)], from="UTF-8")
    if (nzchar(vendor)) {
      fields[["vendor"]] <- vendor
    }
    pos <- pos + size
  }
  count <- uint(pos)
  if (is.na(count) || count < 0) {
    return(fields)
  }
  pos <- pos + 4L
  for (i in seq_len(min(count, .MD_VORBIS_MAX))) {
    size <- uint(pos)
    if (is.na(size) || size <= 0 || pos + 4L + size - 1 > length(bytes)) {
      break
    }
    pos <- pos + 4L
    text <- .mdDecode(bytes[pos:(pos + size - 1)], from="UTF-8")
    pos <- pos + size
    # A comment is a name, an equals sign and a value. Anything else is not
    # one, and is passed over rather than guessed at.
    if (!grepl("=", text, fixed=TRUE)) {
      next
    }
    name <- sub("=.*$", "", text)
    value <- sub("^[^=]*=", "", text)
    if (nzchar(name) && nzchar(value)) {
      fields[[.mdUniqueName(fields, name)]] <- value
    }
  }
  return(fields)
}

#' Read the Vorbis comment block of a FLAC file
#'
#' Walks the metadata blocks at the start of the file, reading only the one
#' that holds the comments. The others hold the stream information, a seek
#' table and often embedded artwork, which can be far larger than everything
#' else in the file put together.
#'
#' @param read A function of a position and a number of bytes
#' @param size The size of the whole file
#' @return A named list of fields
#' @keywords internal
#' @noRd
.vorbisFromFlac <- function(read, size) {
  # The four bytes of the "fLaC" marker come first, so the blocks start at the
  # fifth.
  pos <- 5
  for (i in seq_len(.MD_FLAC_BLOCKS)) {
    header <- read(pos, 4L)
    if (length(header) < 4L) {
      break
    }
    flag <- as.integer(header[1])
    length3 <- .rawToUIntBE(header[2:4])
    if (bitwAnd(flag, 127L) == 4L) {
      if (length3 < 1 || length3 > .MD_BLOCK_MAX) {
        break
      }
      return(.vorbisFields(read(pos + 4L, length3)))
    }
    if (bitwAnd(flag, 128L) != 0L) {
      break
    }
    pos <- pos + 4L + length3
    if (!is.na(size) && pos > size) {
      break
    }
  }
  return(list())
}

#' Read the Vorbis comment block of an Ogg file
#'
#' The comments sit in the second packet of the stream, behind the signature
#' that says what the stream holds. The packet is found by that signature
#' rather than by walking the pages: a comment block large enough to be split
#' across pages, which in practice means one carrying embedded artwork, is
#' therefore not read.
#'
#' @param bytes The start of the file
#' @return A named list of fields
#' @keywords internal
#' @noRd
.vorbisFromOgg <- function(bytes) {
  signatures <- list(
    c(as.raw(3L), charToRaw("vorbis")),
    charToRaw("OpusTags")
  )
  for (signature in signatures) {
    at <- .mdFindBytes(bytes, signature)
    if (length(at) > 0L) {
      return(.vorbisFields(bytes, at[1] + length(signature)))
    }
  }
  return(list())
}

#' Read the Vorbis comments of a file
#' @keywords internal
#' @noRd
.vorbisFromFile <- function(file) {
  head <- .mdFileHead(file)
  container <- .mdContainer(head)
  if (identical(container, "FLAC")) {
    con <- file(file, "rb")
    on.exit(close(con))
    fields <- .vorbisFromFlac(.wavConReader(con), file.size(file))
    return(.mdObject(fields, "vorbisComment"))
  }
  if (identical(container, "Ogg")) {
    return(.mdObject(.vorbisFromOgg(head), "vorbisComment"))
  }
  return(.mdObject(list(), "vorbisComment"))
}

#' Read the Vorbis comments held in raw bytes
#' @keywords internal
#' @noRd
.vorbisFromBytes <- function(bytes) {
  container <- .mdContainer(bytes)
  if (identical(container, "FLAC")) {
    return(.mdObject(
      .vorbisFromFlac(.mdBytesReader(bytes), length(bytes)),
      "vorbisComment"
    ))
  }
  if (identical(container, "Ogg")) {
    return(.mdObject(.vorbisFromOgg(bytes), "vorbisComment"))
  }
  return(.mdObject(list(), "vorbisComment"))
}

#' Read Vorbis comments
#'
#' Reads the Vorbis comments of a FLAC or Ogg file, the metadata those formats
#' carry: `TITLE`, `ARTIST`, `DATE`, `LOCATION` and `COMMENT` among the names
#' in common use, and whatever else the software that wrote the file chose to
#' add. A file holding no comments gives an empty result rather than an error.
#'
#' Fields are named as the file names them, which is a convention rather than a
#' specification: the format reserves no names and a writer may use any. The
#' vendor string, which names the software that encoded the audio, is reported
#' as `vendor`.
#'
#' @param x Path to a FLAC or Ogg file, a Wave object with Vorbis comments
#'   attached, a raw vector holding a file, or a vorbisComment object
#' @return A vorbisComment object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once.
#' @examples
#' \dontrun{
#' m <- vorbisComment("recording.flac")
#' m[["TITLE"]]
#' }
#'
vorbisComment <- function(x) {
  return(.mdDispatch(
    x, "vorbisComment",
    fromFile=.vorbisFromFile,
    fromBytes=.vorbisFromBytes
  ))
}
