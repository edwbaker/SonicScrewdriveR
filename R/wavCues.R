# The cue points of a wave file, and the labels that belong to them.
#
# The two halves of an annotation are written in different chunks. A cue chunk
# holds positions and nothing else. The text that names those positions is in a
# list of kind adtl, whose every field begins with the identifier of the cue
# point it belongs to. wavInfo() reads the text of those fields and passes over
# the identifier, which is right for a list of information about the recording
# as a whole but leaves a label with nothing to attach it to: a file in a cached
# copy of bio.acousti.ca says "Phrase 1" without saying that the phrase begins
# at the start of the recording and runs for ten minutes.

#' A cue point of a wave file is 24 bytes
#' @keywords internal
#' @noRd
.CUE_POINT <- 24L

#' Read the cue points and labels of a wave file
#'
#' A wave file marks positions in a recording with cue points, and names them
#' in a separate list, which is how an editor saves the markers and labelled
#' regions someone has put on a recording. This reads both and gives them back
#' together, one row per cue point, in the order the file holds them.
#'
#' The position of a cue point is a sample offset into the audio, counted in
#' frames rather than bytes, and `seconds` converts it by the sample rate the
#' file declares. `chunk` is the chunk the offset is into, which is `data` for
#' an ordinary recording; a file that keeps its audio in a wave list can point
#' a cue at one part of it, and then the offset is into that part rather than
#' into the recording.
#'
#' A labelled region carries a `length` in samples and a `purpose`, commonly
#' `rgn`, where a plain marker carries neither. `label` is the short name of a
#' point, `note` a longer comment on it, and `text` the text of a labelled
#' region, each as the file's own list gives it.
#'
#' A label whose cue point is missing is still reported, with no position: the
#' text is metadata the file carries, and dropping it because the chunk that
#' places it has been lost would be reporting less than the file holds. A cue
#' point with no label is likewise reported with no text; nine files in a
#' cached copy of bio.acousti.ca declare 99 such points, all of them at
#' position zero, which is a writer reserving the room rather than marking
#' anything.
#'
#' By default each cue point is given as an [Annotation][Annotation-class]
#' object. A labelled region is an annotation of type `region` running from its
#' position for its length, and any other point an annotation of type `cue`
#' that starts and ends at its position. The value of the annotation is the
#' label of the point, or failing that the text of its region; its source is
#' `wavCues`, its file the path read, and its metadata the fields of its row
#' in the data frame other than `seconds`. An annotation whose position is not
#' known, or not known in seconds, starts and ends at `NA`.
#'
#' @param x Path to a wave file, or a raw vector holding one
#' @param output One of "annotations" or "data.frame".
#' @return A list of Annotation objects, one per cue point, or with `output`
#'   "data.frame" a data frame of one row per cue point, with its identifier,
#'   position, position in seconds, label, note, region length, purpose, region
#'   text and the chunk its position is into. A file holding no cue points and
#'   no labels gives an empty list, or a data frame of no rows.
#' @export
#' @seealso [wavChunks()] lists the chunks a file holds, and [wavInfo()] reads
#'   the text of the same labels without the positions they belong to.
#'   [writeAudacityLabels()] writes the annotations as an Audacity label file.
#' @examples
#' \dontrun{
#' cues <- wavCues("marked.wav", output="data.frame")
#' cues[!is.na(cues$label), c("seconds", "label")]
#' writeAudacityLabels(wavCues("marked.wav"), "marked.txt")
#' }
#'
wavCues <- function(x, output="annotations") {
  .validateChoice(output, c("annotations", "data.frame"), msg="Unknown output format.")
  if (is.raw(x)) {
    return(.wavCueFrame(.wavChunks(x), function(at, n) x[at:(at + n - 1L)], output, ""))
  }
  .mdCheckFile(x)
  con <- file(x, "rb")
  on.exit(close(con))
  size <- file.size(x)
  read <- .wavConReader(con)
  return(.wavCueFrame(.wavWalk(read, size, size), read, output, x))
}

#' Build what wavCues() returns
#'
#' @param walked What .wavWalk() returned
#' @param read A function of a one-based position and a number of bytes
#' @param output One of "annotations" or "data.frame"
#' @param path The path of the file, or "" for bytes given directly
#' @return A list of Annotation objects, or a data frame of one row per cue point
#' @keywords internal
#' @noRd
.wavCueFrame <- function(walked, read, output, path) {
  body <- function(id, cap=.MD_BLOCK_MAX) {
    chunk <- .wavFindChunk(walked$chunks, id)
    if (is.null(chunk) || chunk$size < 1) {
      return(NULL)
    }
    return(read(chunk$start, min(chunk$size, cap)))
  }
  fmt <- body("fmt ")
  rate <- NA_real_
  if (!is.null(fmt)) {
    fields <- .wavFmtFields(fmt, length(fmt))
    if (!is.null(fields) && fields$sampleRate > 0) {
      rate <- fields$sampleRate
    }
  }
  points <- .wavCuePoints(body("cue "))
  labels <- list()
  for (chunk in walked$chunks) {
    if (!chunk$id %in% .WAV_LIST_IDS || chunk$size < 4L) {
      next
    }
    one <- read(chunk$start, min(chunk$size, .MD_BLOCK_MAX))
    if (.rawToId(one[1:4]) == "adtl") {
      labels <- .wavCueLabels(one, labels)
    }
  }
  cues <- .wavCueJoin(points, labels, rate)
  if (output == "data.frame") {
    return(cues)
  }
  return(.wavCueAnnotations(cues, rate, path))
}

#' Make the cue points of a wave file into Annotation objects
#'
#' @param cues The data frame of cue points
#' @param rate The sample rate the file declares, or NA
#' @param path The path of the file, or ""
#' @return A list of Annotation objects
#' @keywords internal
#' @noRd
.wavCueAnnotations <- function(cues, rate, path) {
  fields <- c("id", "position", "label", "note", "length", "purpose", "text", "chunk")
  lapply(seq_len(nrow(cues)), function(i) {
    cue <- cues[i, ]
    region <- !is.na(cue$length)
    annotation(
      file = path,
      start = cue$seconds,
      end = cue$seconds + if (region) cue$length / rate else 0,
      source = "wavCues",
      type = if (region) "region" else "cue",
      value = Find(Negate(is.na), c(cue$label, cue$text, "")),
      metadata = as.list(cue[fields])
    )
  })
}

#' Read the points of a cue chunk
#'
#' Each point is an identifier, the position in the play order, the chunk the
#' position is into, that chunk's start, the block start, and the sample offset
#' within the chunk. The sample offset is the position in the audio, and is
#' what is reported: for an ordinary recording a writer sets both to the same
#' value, and where they differ it is the offset that says where in the samples
#' the point is.
#'
#' A chunk declaring more points than it has room for is read as far as it
#' goes, as a file cut short is read.
#'
#' @param body The body of a cue chunk, or NULL
#' @return A list of points, each a list of id, position and chunk
#' @keywords internal
#' @noRd
.wavCuePoints <- function(body) {
  if (is.null(body) || length(body) < 4L + .CUE_POINT) {
    return(list())
  }
  declared <- .rawToUInt(body[1:4])
  room <- (length(body) - 4L) %/% .CUE_POINT
  points <- list()
  for (i in seq_len(min(declared, room))) {
    at <- 4L + (i - 1L) * .CUE_POINT
    points[[i]] <- list(
      id = .rawToUInt(body[(at + 1L):(at + 4L)]),
      chunk = .rawToId(body[(at + 9L):(at + 12L)]),
      position = .rawToUInt(body[(at + 21L):(at + 24L)])
    )
  }
  return(points)
}

#' Read the labels of an associated data list
#'
#' Every field of the list begins with the identifier of the cue point it
#' belongs to. A labelled text field carries a whole header besides: the length
#' of the region in samples, what it is for, and the country, language, dialect
#' and code page its text is written for.
#'
#' @param body The body of a LIST chunk of kind adtl
#' @param labels The labels read from earlier lists, keyed by cue identifier
#' @return The labels, with those of this list added
#' @keywords internal
#' @noRd
.wavCueLabels <- function(body, labels=list()) {
  pos <- 5L
  while (pos + 7L <= length(body)) {
    id <- .rawToId(body[pos:(pos + 3L)])
    size <- .rawToUInt(body[(pos + 4L):(pos + 7L)])
    if (size < 4L || pos + 8L + size - 1 > length(body)) {
      break
    }
    field <- body[(pos + 8L):(pos + 8L + size - 1)]
    cue <- as.character(.rawToUInt(field[1:4]))
    held <- if (is.null(labels[[cue]])) list() else labels[[cue]]
    skip <- if (id == "ltxt") 20L else 4L
    text <- if (size > skip) .mdDecode(field[(skip + 1L):length(field)]) else ""
    if (id == "ltxt" && length(field) >= 12L) {
      held$length <- .rawToUInt(field[5:8])
      held$purpose <- .rawToId(field[9:12])
      held$text <- text
    } else if (id %in% c("labl", "note")) {
      # A list may name the same point twice, which is the file disagreeing
      # with itself; the first is kept, as the fields of a list are elsewhere.
      if (is.null(held[[id]])) {
        held[[id]] <- text
      }
    }
    labels[[cue]] <- held
    # The fields of a list are padded to an even number of bytes, as chunks are.
    pos <- pos + 8L + size + (size %% 2)
  }
  return(labels)
}

#' Put the positions and the labels back together
#'
#' @param points The points of the cue chunk
#' @param labels The labels of the associated data lists
#' @param rate The sample rate the file declares, or NA
#' @return A data frame of one row per cue point, then one per label that
#'   belongs to no point
#' @keywords internal
#' @noRd
.wavCueJoin <- function(points, labels, rate) {
  ids <- vapply(points, function(p) as.character(p$id), character(1))
  orphans <- setdiff(names(labels), ids)
  rows <- c(
    lapply(points, function(p) .wavCueRow(p$id, p$position, p$chunk,
                                          labels[[as.character(p$id)]], rate)),
    lapply(orphans, function(cue) .wavCueRow(as.numeric(cue), NA_real_,
                                             NA_character_, labels[[cue]], rate))
  )
  if (length(rows) == 0L) {
    rows <- list(.wavCueRow(numeric(0), numeric(0), character(0), NULL, rate))
  }
  return(do.call(rbind, rows))
}

#' One row of the cue point table
#'
#' @param id The cue identifier
#' @param position The sample offset, or NA for a label with no point
#' @param chunk The chunk the offset is into, or NA
#' @param held What the lists said about this point, or NULL
#' @param rate The sample rate the file declares, or NA
#' @return A one-row data frame
#' @keywords internal
#' @noRd
.wavCueRow <- function(id, position, chunk, held, rate) {
  pick <- function(name, empty) {
    if (is.null(held) || is.null(held[[name]]) || !nzchar(held[[name]])) {
      return(empty)
    }
    return(held[[name]])
  }
  n <- length(id)
  return(data.frame(
    id = as.numeric(id),
    position = as.numeric(position),
    seconds = if (n == 0L) numeric(0) else as.numeric(position) / rate,
    label = if (n == 0L) character(0) else pick("labl", NA_character_),
    note = if (n == 0L) character(0) else pick("note", NA_character_),
    length = if (n == 0L || is.null(held) || is.null(held$length)) {
      rep(NA_real_, n)
    } else {
      as.numeric(held$length)
    },
    purpose = if (n == 0L) character(0) else pick("purpose", NA_character_),
    text = if (n == 0L) character(0) else pick("text", NA_character_),
    chunk = as.character(chunk),
    stringsAsFactors = FALSE
  ))
}
