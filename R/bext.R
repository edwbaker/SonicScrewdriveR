# Broadcast Wave metadata, held in a wave file as a "bext" chunk.
#
# Unlike the other metadata chunks, bext is a fixed layout rather than a run of
# named fields: a description, an originator, a reference, a date and a time,
# each a run of bytes of a stated width padded with nuls, then a sample count,
# a version, a unique identifier and, from the end of the fixed part to the end
# of the chunk, the coding history.
#
# The format is an EBU specification written for broadcasters, but it is what
# an archive of recordings tends to be handed, and the sound libraries this
# package was written against put the whole of their record for a recording
# into its description.

#' Widths of the fixed fields of a bext chunk
#'
#' Each is the position of its first byte in the chunk and the number of bytes
#' it occupies, in the order the specification lays them out.
#' @keywords internal
#' @noRd
.BEXT_FIELDS <- list(
  Description = c(1L, 256L),
  Originator = c(257L, 32L),
  OriginatorReference = c(289L, 32L),
  OriginationDate = c(321L, 10L),
  OriginationTime = c(331L, 8L)
)

#' Position of the first byte of the coding history
#'
#' The fixed part of the chunk runs to 602 bytes: the text fields, the sample
#' count the recording starts at, the version, the unique material identifier
#' and the loudness values added by version 2.
#' @keywords internal
#' @noRd
.BEXT_FIXED <- 602L

#' Read the date and time a bext chunk records as one time
#'
#' The two fields are written as a local date and time with nothing to say
#' which timezone that was, so they are read in the timezone of the session.
#'
#' @param date The OriginationDate field
#' @param time The OriginationTime field
#' @return A POSIXct, or NULL where the two do not spell a time
#' @keywords internal
#' @noRd
.bextDatetime <- function(date, time) {
  if (!grepl("^[0-9]{4}[-:_/.][0-9]{2}[-:_/.][0-9]{2}$", date)) {
    return(NULL)
  }
  # The specification asks for hyphens between the parts of the date and
  # colons between the parts of the time, but allows any punctuation, and
  # writers differ.
  day <- gsub("[-:_/.]", "-", date)
  clock <- gsub("[-:_/.]", ":", time)
  parsed <- as.POSIXct(paste(day, clock), format="%Y-%m-%d %H:%M:%S", tz="")
  if (is.na(parsed)) {
    # A writer that cut the seconds off a field the specification gives eight
    # characters to has still said what hour and minute the recording was made
    # in, which is worth more than the midnight below. The whole time is tried
    # first because a time read for its hour and minute alone stops at the
    # minute and says nothing of what follows it, so asking that of a whole
    # time would quietly drop the seconds it plainly gives.
    parsed <- as.POSIXct(paste(day, clock), format="%Y-%m-%d %H:%M", tz="")
  }
  if (is.na(parsed)) {
    # A time written in no layout the specification allows says nothing about
    # the date beside it, which the file gave plainly, so the date is kept at
    # midnight rather than thrown away with it. That is what a file leaving the
    # time empty already gave, and a writer's bad time should cost no more than
    # a missing one.
    parsed <- as.POSIXct(day, format="%Y-%m-%d", tz="")
  }
  if (is.na(parsed)) {
    return(NULL)
  }
  return(parsed)
}

#' Read the unique material identifier of a bext chunk
#'
#' Sixty-four bytes which are either an identifier or, far more often, unused
#' and left as zeroes. It is reported as the hexadecimal it is written in,
#' rather than as text, which is not what it holds.
#'
#' @param bytes The UMID field
#' @return A string, or "" where the field is unused
#' @keywords internal
#' @noRd
.bextUmid <- function(bytes) {
  if (length(bytes) == 0L || !any(as.integer(bytes) != 0L)) {
    return("")
  }
  return(toupper(paste(format(bytes), collapse="")))
}

#' Read the fields of a bext chunk
#'
#' @param body The body of a bext chunk
#' @return A named list of fields
#' @keywords internal
#' @noRd
.bextFields <- function(body) {
  # A chunk cut short by a recording that stopped early still holds the start
  # of whatever its fields had got to, so a field is read as far as the chunk
  # goes rather than refused for running past the end of it.
  field <- function(at, size) {
    if (at > length(body)) {
      return("")
    }
    return(.mdDecode(body[at:min(at + size - 1L, length(body))]))
  }

  fields <- list()
  for (name in names(.BEXT_FIELDS)) {
    where <- .BEXT_FIELDS[[name]]
    value <- field(where[1], where[2])
    if (nzchar(value)) {
      fields[[name]] <- value
    }
  }

  datetime <- .bextDatetime(
    if (is.null(fields$OriginationDate)) "" else fields$OriginationDate,
    if (is.null(fields$OriginationTime)) "00:00:00" else fields$OriginationTime
  )
  if (!is.null(datetime)) {
    fields$datetime <- datetime
  }

  # The sample the recording starts at, counted from midnight, which is how a
  # broadcaster records where a file sits in a day's recording. It is written
  # as two 32-bit halves rather than as one 64-bit number.
  if (length(body) >= 346L) {
    reference <- .rawToUInt(body[339:346])
    if (reference > 0) {
      fields$TimeReference <- reference
    }
  }
  if (length(body) >= 348L) {
    fields$Version <- as.integer(.rawToUInt(body[347:348]))
  }
  if (length(body) >= 412L) {
    umid <- .bextUmid(body[349:412])
    if (nzchar(umid)) {
      fields$UMID <- umid
    }
  }
  if (length(body) > .BEXT_FIXED) {
    history <- field(.BEXT_FIXED + 1L, length(body) - .BEXT_FIXED)
    if (nzchar(history)) {
      fields$CodingHistory <- history
    }
  }
  return(fields)
}

#' Read Broadcast Wave metadata
#'
#' Reads the bext chunk of a wave file, the Broadcast Wave metadata written by
#' recorders and editing software built for broadcast and by the archives that
#' take their files. The description is a free run of text, and a sound library
#' commonly puts the whole of its record for a recording into it, as in
#' "Acinipe calabra; Studio recording; Italy, Sicily, Baucina (PA); Female
#' song". A file holding no bext chunk gives an empty result rather than an
#' error.
#'
#' Fields are named as the specification names them. The date and time are also
#' reported together as `datetime`; the file says nothing about which timezone
#' they were written in, so they are read in the timezone of the session. A
#' time written in no layout the specification allows leaves the date at
#' midnight, which is what a file that left the time empty already gave, rather
#' than taking the date down with it.
#'
#' @param x Path to a wave file, a Wave object with bext metadata attached, a
#'   raw vector holding a wave file, or a bext object
#' @return A bext object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once, and [ixml()] reads the iXML chunk that a recorder writing
#'   this format often writes beside it.
#' @references
#'   European Broadcasting Union (2011). Specification of the Broadcast Wave
#'   Format: a format for audio data files in broadcasting. EBU Tech 3285.
#'   \url{https://tech.ebu.ch/publications/tech3285}
#' @examples
#' \dontrun{
#' m <- bext("recording.wav")
#' m[["Description"]]
#' }
#'
bext <- function(x) {
  return(.mdDispatch(
    x, "bext",
    fromFile=function(file) .mdBextObject(.mdWavChunk(file, "bext")),
    fromBytes=function(bytes) .mdBextObject(.mdWavChunkBytes(bytes, "bext"))
  ))
}

#' Build a bext object from the body of a bext chunk
#' @keywords internal
#' @noRd
.mdBextObject <- function(body) {
  if (is.null(body) || length(body) == 0L) {
    return(.mdObject(list(), "bext"))
  }
  return(.mdObject(.bextFields(body), "bext"))
}
