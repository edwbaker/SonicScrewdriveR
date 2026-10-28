# One walk of a wave file, shared.
#
# Each metadata reader here looks for a chunk of its own, and reading a file
# through all of them walked it once per reader: a file holding GUANO, wamd,
# bext, iXML, a list, an XMP packet, a display name and an ID3 tag was opened
# and walked eight times over to read eight chunks of it. The walk is the same
# every time, so it is done once and the result passed to the readers.
#
# The map a walk produces is worth having in its own right. A reader can only
# report the chunks it knows; the map reports what the file holds, including
# the chunks nothing here reads, and where they are, so that a caller can look
# at one for themselves.

#' List the RIFF chunks a wave file holds
#'
#' A wave file is a sequence of chunks, each named by four characters, and what
#' a recorder or an editor wrote into one is what the readers in this package
#' go looking for. This reports the chunks themselves: every chunk the file
#' holds, in the order it holds them, whether or not anything here can read it.
#'
#' Only the chunk headers are read, each seeking past the body it describes, so
#' listing the chunks of a recording costs the same whether it is a second long
#' or an hour. Reading the file as a path is the cheaper way round for that
#' reason; a raw vector is walked where it sits.
#'
#' A chunk whose declared size runs past the end of the file is reported with
#' the size it actually has and flagged as truncated, rather than refused, as
#' recordings cut short by a flat battery or a full card are common. A file over
#' 4 GiB written in the RF64 or BW64 form is walked by the sizes in its `ds64`
#' chunk, as [readAudio()] reads it.
#'
#' @param x Path to a wave file, or a raw vector holding one
#' @return A data frame of one row per chunk, with the chunk's identifier, the
#'   one-based position of the first byte of its body, its size in bytes,
#'   whether it was truncated, and, for a `LIST` chunk, the kind of list it is.
#'   The form of the file, its RIFF size and the path it was read from are
#'   attached as attributes.
#' @export
#' @examples
#' f <- system.file("extdata", "AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
#' wavChunks(f)
#'
#' # The chunks nothing in this package reads are listed too, so a file can be
#' # looked at before a reader for it exists.
#' chunks <- wavChunks(f)
#' chunks[!chunks$id %in% c("fmt ", "data"), ]
#'
wavChunks <- function(x) {
  if (is.raw(x)) {
    walked <- .wavChunks(x)
    read <- function(at, n) x[at:(at + n - 1L)]
    return(.wavChunkFrame(walked, read, NA_character_))
  }
  .mdCheckFile(x)
  con <- file(x, "rb")
  on.exit(close(con))
  size <- file.size(x)
  read <- .wavConReader(con)
  return(.wavChunkFrame(.wavWalk(read, size, size), read, x))
}

#' Build the data frame that wavChunks() returns
#'
#' The kind of a list is the first four bytes of its body rather than part of
#' its header, so it costs a read of its own. Four bytes per list is worth
#' paying: a file's lists are read differently depending on their kind, and a
#' map that does not say which is which cannot be acted on.
#'
#' @param walked What .wavWalk() returned
#' @param read A function of a one-based position and a number of bytes
#' @param file The path walked, or NA for raw bytes
#' @return A data frame of one row per chunk
#' @keywords internal
#' @noRd
.wavChunkFrame <- function(walked, read, file) {
  chunks <- walked$chunks
  form <- vapply(chunks, function(chunk) {
    if (!chunk$id %in% .WAV_LIST_IDS || chunk$size < 4L) {
      return(NA_character_)
    }
    return(.rawToId(read(chunk$start, 4L)))
  }, character(1))
  out <- data.frame(
    id = vapply(chunks, function(chunk) chunk$id, character(1)),
    start = vapply(chunks, function(chunk) as.numeric(chunk$start), numeric(1)),
    size = vapply(chunks, function(chunk) as.numeric(chunk$size), numeric(1)),
    truncated = vapply(chunks, function(chunk) chunk$truncated, logical(1)),
    form = form,
    stringsAsFactors = FALSE
  )
  return(structure(
    out,
    class = c("wavChunks", "data.frame"),
    wavForm = walked$form,
    riffSize = walked$riffSize,
    file = file
  ))
}

#' Print the chunks of a wave file
#'
#' @param x A wavChunks data frame
#' @param ... Passed to the data frame's own print method
#' @return `x`, invisibly
#' @export
print.wavChunks <- function(x, ...) {
  cat(attr(x, "wavForm"), "WAVE,", nrow(x),
      if (nrow(x) == 1L) "chunk" else "chunks", "\n")
  print.data.frame(x, ...)
  return(invisible(x))
}

# -------------------------------------------------------------- the map ------

#' Walk a wave file once, keeping what the readers will ask for
#'
#' The readers ask for the body of a chunk by its identifier, so the bodies are
#' read while the file is open and handed over from memory afterwards. The audio
#' is left where it is: a reader that wanted the data chunk would be reading the
#' recording, which is not what any of these do.
#'
#' A chunk too large to be metadata is left unread rather than capped, so that
#' asking the map for it falls through to the file and gets exactly what reading
#' the file would have given. Nothing is quietly truncated by having been cached.
#'
#' @param file Path to a wave file
#' @param cap Most bytes to keep for any one chunk
#' @return An object of class wavChunkMap, or NULL where the file is not a wave
#'   file this package can walk
#' @keywords internal
#' @noRd
.mdWavMap <- function(file, cap=.MD_BLOCK_MAX) {
  # Asked for a file that is not there, or for a directory, say so by having no
  # map rather than by opening it: the caller still has the path, and whatever
  # it does with it will raise the error that belongs to it. Opening a directory
  # warns about permissions before it fails, which says nothing about what was
  # actually wrong.
  if (!isTRUE(file.exists(file)) || dir.exists(file)) {
    return(NULL)
  }
  return(tryCatch({
    con <- file(file, "rb")
    on.exit(close(con))
    size <- file.size(file)
    read <- .wavConReader(con)
    walked <- .wavWalk(read, size, size)
    bodies <- lapply(walked$chunks, function(chunk) {
      if (chunk$id == "data" || chunk$size < 1 || chunk$size > cap) {
        return(NULL)
      }
      return(read(chunk$start, chunk$size))
    })
    structure(
      list(file=file, size=size, head=read(1L, .MD_HEAD_READ),
           chunks=walked$chunks, bodies=bodies),
      class = "wavChunkMap"
    )
  }, unsupportedWav=function(cond) NULL, error=function(cond) NULL))
}

#' The body of one chunk, from the map where it is there and the file where not
#'
#' @param map A wavChunkMap
#' @param id The chunk identifier to look for
#' @param cap Most bytes to give back
#' @return A raw vector, or NULL where the map holds no such chunk or it holds
#'   nothing
#' @keywords internal
#' @noRd
.mdMapChunk <- function(map, id, cap) {
  for (i in seq_along(map$chunks)) {
    chunk <- map$chunks[[i]]
    if (chunk$id != id || chunk$size < 1) {
      next
    }
    return(.mdMapBody(map, i, cap))
  }
  return(NULL)
}

#' The bodies of every chunk of one identifier
#' @keywords internal
#' @noRd
.mdMapChunks <- function(map, id, cap) {
  bodies <- list()
  for (i in seq_along(map$chunks)) {
    chunk <- map$chunks[[i]]
    if (!chunk$id %in% id || chunk$size < 1) {
      next
    }
    body <- .mdMapBody(map, i, cap)
    if (!is.null(body)) {
      bodies[[length(bodies) + 1L]] <- body
    }
  }
  return(bodies)
}

#' One chunk body out of the map
#'
#' A body kept in the map is whole, so a smaller cap than it was read under is
#' applied here rather than sending the caller back to the file for bytes that
#' are already in memory.
#'
#' @param map A wavChunkMap
#' @param i Which chunk
#' @param cap Most bytes to give back
#' @return A raw vector, or NULL
#' @keywords internal
#' @noRd
.mdMapBody <- function(map, i, cap) {
  chunk <- map$chunks[[i]]
  body <- map$bodies[[i]]
  if (!is.null(body)) {
    return(if (length(body) > cap) body[seq_len(cap)] else body)
  }
  return(.mdFileRange(map$file, chunk$start, min(chunk$size, cap)))
}
