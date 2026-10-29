# The olym chunk of a wave file: what an Olympus recorder wrote about the
# recording.
#
# The chunk is a fixed layout rather than a run of named fields, so every field
# is at a known offset and nothing in the chunk says what it holds. ExifTool
# documents the layout for the LS-5, LS-10 and LS-11, and the offsets below are
# that documentation checked against the 397 files carrying the chunk in cached
# copies of the TSA and iNaturalist corpora: 217 written in one form and 180 in
# another, from the LS-3, LS-5, LS-10, LS-11, LS-12, LS-100, DM-550, DM-620 and
# DM-720.
#
# The two forms are told apart by the three bytes at 0x01, which read "dss" in
# the older and "wav" in the newer, and they differ in where the duration is:
# the older writes it at 0x200, where the newer leaves 0xffffffff and writes it
# at 0x56 instead. Across those 397 files the split is exact, with no file
# carrying both. The index marks at 0x20a are likewise written only by the
# older form, which is the one ExifTool was written against.
#
# A handful of bytes vary from file to file and are not read, because nothing
# says what they mean and the corpora do not: 0x22 and 0x24, a single byte at
# 0x5a holding 0, "0", "1" or "3", and a byte at 0x2a4 of the older form whose
# value goes with the model of the recorder rather than with the recording.

#' Where each field of an olym chunk is, as a zero-based offset into the chunk
#'
#' `durationOld` is where the older form of the chunk writes the duration and
#' `duration` where the newer one does. Both are read, as a file carries one or
#' the other and which it carries is not worth reporting.
#' @keywords internal
#' @noRd
.OLYM_AT <- list(
  magic = c(0x01, 3),
  model = c(0x0c, 16),
  fileNumber = c(0x1c, 4),
  timestamp = c(0x26, 12),
  timestampEnd = c(0x32, 12),
  recordingTime = c(0x3e, 6),
  duration = c(0x56, 4),
  text = c(0x6a, 100),
  text2 = c(0xd4, 100),
  durationOld = c(0x200, 4)
)

#' Where the index marks of an olym chunk are
#'
#' Sixteen slots, ten bytes apart, each holding four bytes of milliseconds.
#' @keywords internal
#' @noRd
.OLYM_MARKS <- seq(0x20a, by = 10, length.out = 16)

#' The value an olym field holds when it holds nothing
#' @keywords internal
#' @noRd
.OLYM_EMPTY <- 4294967295

#' Read bytes from an olym chunk by offset
#'
#' A chunk shorter than the offset asked for gives nothing rather than an
#' error, so that a truncated chunk is read as far as it goes.
#'
#' @param body The chunk
#' @param where A length-two vector of a zero-based offset and a length
#' @return The bytes, or an empty raw vector where the chunk does not reach
#' @keywords internal
#' @noRd
.olymAt <- function(body, where) {
  if (length(body) < where[1] + where[2]) {
    return(raw(0))
  }
  return(body[where[1] + seq_len(where[2])])
}

#' Read a number an olym chunk holds
#'
#' @param bytes The bytes of the field
#' @return The number, or NULL where the field is absent or holds nothing
#' @keywords internal
#' @noRd
.olymNumber <- function(bytes) {
  if (length(bytes) < 4L) {
    return(NULL)
  }
  value <- .rawToUInt(bytes)
  if (value == .OLYM_EMPTY) {
    return(NULL)
  }
  return(value)
}

#' Read text an olym chunk holds
#'
#' A field holding text is padded to its length with spaces, and one holding
#' nothing is filled with zeroes, with 0xff, or with a run of each: the older
#' form of the chunk fills the first text field with 0xff as far as 0x88 and
#' with zeroes after it, which is neither all of one nor all of the other.
#' Decoding that gives a run of 255s, and a reader that trimmed only spaces
#' would report it as text for every file of that form in the corpora, 217 of
#' 397. Both are padding, so both are dropped from either end before anything
#' is read.
#'
#' They are dropped from the ends rather than wherever they appear, so that a
#' field holding text with a 255 in it keeps the character rather than quietly
#' losing it. A field whose text is nothing but that character is read as
#' padding; no file is known to hold one, and the alternative is to report a
#' field for the 217 that hold nothing.
#'
#' @param bytes The bytes of the field
#' @return The text, or NULL where the field is absent or holds nothing
#' @keywords internal
#' @noRd
.olymText <- function(bytes) {
  padding <- as.integer(bytes) %in% c(0L, 255L)
  written <- which(!padding)
  if (length(written) == 0L) {
    return(NULL)
  }
  text <- trimws(.mdDecode(bytes[written[1]:written[length(written)]],
                           from="latin1"))
  if (!nzchar(text)) {
    return(NULL)
  }
  return(text)
}

#' Read the time an olym chunk gives
#'
#' Written as twelve digits, YYMMDDHHMMSS, with no year beyond the decade and
#' nothing to say which part of the world the clock was set to. The century is
#' decided the way Olympus firmware decides it, a year under 70 being this one.
#' The time is read as a wall clock in the timezone of the session, as a wamd
#' timestamp without an offset is, because the chunk gives no offset at all.
#'
#' A field holding anything other than twelve digits gives nothing. The
#' recorders write a run of 0xff where the clock was never set, which is what
#' this is usually refusing.
#'
#' @param bytes The bytes of the field
#' @return A POSIXct, or NULL where the field holds no time
#' @keywords internal
#' @noRd
.olymTime <- function(bytes) {
  if (length(bytes) < 12L) {
    return(NULL)
  }
  digits <- .mdDecode(bytes, from="latin1")
  if (!grepl("^[0-9]{12}$", digits)) {
    return(NULL)
  }
  year <- as.integer(substr(digits, 1, 2))
  stamp <- paste0(
    if (year < 70) "20" else "19", substr(digits, 1, 2), "-",
    substr(digits, 3, 4), "-", substr(digits, 5, 6), " ",
    substr(digits, 7, 8), ":", substr(digits, 9, 10), ":",
    substr(digits, 11, 12)
  )
  return(.mdDate(stamp))
}

#' Read the length of the recording an olym chunk gives
#'
#' Written as six digits, HHMMSS. The recorders write a run of 0xff rather than
#' zeroes where they have nothing to say, so a field that is not six digits
#' gives nothing.
#'
#' @param bytes The bytes of the field
#' @return The length in seconds, or NULL where the field holds none
#' @keywords internal
#' @noRd
.olymRecordingTime <- function(bytes) {
  if (length(bytes) < 6L) {
    return(NULL)
  }
  digits <- .mdDecode(bytes, from="latin1")
  if (!grepl("^[0-9]{6}$", digits)) {
    return(NULL)
  }
  return(as.numeric(substr(digits, 1, 2)) * 3600 +
         as.numeric(substr(digits, 3, 4)) * 60 +
         as.numeric(substr(digits, 5, 6)))
}

#' Read the index marks an olym chunk holds
#'
#' The marks a recorder was told to put in the recording while it was being
#' made, in milliseconds from the start, in sixteen slots of which an unused
#' one holds 0xffffffff.
#'
#' Nine of the 397 files in the corpora carry a mark, and four of those hold a
#' mark of zero. A mark at the very start of a recording and a slot a firmware
#' filled with zero rather than with 0xffffffff cannot be told apart, and
#' nothing documents either, so a zero is read as the mark it is written as.
#'
#' @param body The chunk
#' @return The marks in seconds, or NULL where there are none
#' @keywords internal
#' @noRd
.olymMarks <- function(body) {
  marks <- numeric(0)
  for (at in .OLYM_MARKS) {
    value <- .olymNumber(.olymAt(body, c(at, 4)))
    if (!is.null(value)) {
      marks <- c(marks, value / 1000)
    }
  }
  if (length(marks) == 0L) {
    return(NULL)
  }
  return(marks)
}

#' Read what an Olympus recorder wrote about a recording
#'
#' Reads the `olym` chunks of a wave file. Olympus recorders write one into
#' every file they make, holding the model of the recorder, the number the
#' recording was given on it, the time it was started and the time it was
#' ended, how long it runs, and any marks that were put in it while it was
#' being made.
#'
#' The chunk is commonly the only metadata such a file carries: a wave file out
#' of an LS or DM recorder has no INFO list, no broadcast metadata and no tag,
#' and before this chunk was read there was nothing in it for this package to
#' find.
#'
#' `duration` and `recordingTime` are two ways the recorder wrote the same
#' thing, the first in milliseconds and the second to the second, and across
#' the 397 files in the corpora they never disagree. Neither is the length of
#' the audio the file now holds: 307 of the 397 agree with the data chunk to
#' within half a second, and of the 90 that do not, 89 say the recording was
#' longer than the audio that is there, which is what a file cut after it came
#' off the recorder looks like.
#'
#' `timestampEnd` is not the time the audio ends either. It is the time the
#' recorder stopped, which includes however long it was paused for: in 152 of
#' the 397 it is further from the start than the recording is long, by as much
#' as thirteen hours.
#'
#' Two fields hold a hundred characters of text each, and nothing documents
#' what either is for. One file of the 397 writes in them, putting the subject
#' of the recording in the second, so they are read and reported under the
#' names `text` and `text.2`, by the slot they came from rather than by any
#' meaning, and neither is taken for the title of the recording.
#'
#' @param x Path to a wave file, a Wave object with olym metadata attached, a
#'   raw vector holding a wave file, or an olym object
#' @return An olym object, which is a named list of the fields the chunk held:
#'   `model`, `fileNumber`, `timestamp`, `timestampEnd`, `recordingTime` and
#'   `duration` in seconds, `marks` in seconds from the start of the recording,
#'   and `text` and `text.2`. A file holding more than one olym chunk gives the
#'   fields of the second under names ending in a further number.
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once, and [wavCues()] reads the marks an editor wrote into a
#'   recording afterwards, which the recorder's own marks are separate from.
#' @examples
#' \dontrun{
#' olym("recording.wav")
#' }
#'
olym <- function(x) {
  return(.mdDispatch(
    x, "olym",
    fromFile=function(file) .mdOlymObject(.mdWavChunks(file, "olym")),
    fromBytes=function(bytes) .mdOlymObject(.mdWavChunksBytes(bytes, "olym"))
  ))
}

#' Build an olym object from the bodies of the olym chunks of a file
#' @keywords internal
#' @noRd
.mdOlymObject <- function(bodies) {
  fields <- list()
  for (body in bodies) {
    for (field in .mdOlymFields(body)) {
      fields[[.mdUniqueName(fields, field$name)]] <- field$value
    }
  }
  return(.mdObject(fields, "olym"))
}

#' Read the fields of one olym chunk
#'
#' Given as a list of name and value rather than as a named list, so that the
#' two text fields keep the names of the slots they came from where only the
#' second of them was written.
#'
#' @param body The chunk
#' @return A list of fields, each a name and a value
#' @keywords internal
#' @noRd
.mdOlymFields <- function(body) {
  read <- function(name) .olymAt(body, .OLYM_AT[[name]])
  # One form of the chunk holds the duration where the other holds nothing.
  duration <- .olymNumber(read("duration"))
  if (is.null(duration)) {
    duration <- .olymNumber(read("durationOld"))
  }
  found <- list(
    model = .olymText(read("model")),
    fileNumber = .olymNumber(read("fileNumber")),
    timestamp = .olymTime(read("timestamp")),
    timestampEnd = .olymTime(read("timestampEnd")),
    recordingTime = .olymRecordingTime(read("recordingTime")),
    duration = if (is.null(duration)) NULL else duration / 1000,
    marks = .olymMarks(body),
    text = .olymText(read("text")),
    text.2 = .olymText(read("text2"))
  )
  held <- !vapply(found, is.null, logical(1))
  return(lapply(names(found)[held], function(name) {
    list(name=name, value=found[[name]])
  }))
}
