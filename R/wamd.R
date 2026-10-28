# Wildlife Acoustics metadata, held in a wave file as a "wamd" chunk.
#
# The chunk is a run of fields, each a two-byte identifier, a four-byte size
# and that many bytes of value, all little-endian and with no padding between
# them. The identifiers are numbers rather than names, so the names below are
# those of the reference implementation published with the GUANO specification,
# which was written against the format as Wildlife Acoustics devices write it.

#' Names of the numbered fields of a wamd chunk
#'
#' Taken from the reference implementation of the conversion from this format
#' to GUANO, published with the GUANO specification.
#' @keywords internal
#' @noRd
.WAMD_IDS <- c(
  "0"  = "version",
  "1"  = "model",
  "2"  = "serial",
  "3"  = "firmware",
  "4"  = "prefix",
  "5"  = "timestamp",
  "6"  = "gpsfirst",
  "7"  = "gpstrack",
  "8"  = "software",
  "9"  = "license",
  "10" = "notes",
  "11" = "auto_id",
  "12" = "manual_id",
  "13" = "voicenotes",
  "14" = "auto_id_stats",
  "15" = "time_expansion",
  "16" = "program",
  "17" = "runstate",
  "18" = "microphone",
  "19" = "sensitivity"
)

#' Fields of a wamd chunk that hold something other than text
#'
#' An embedded voice recording, the program a device was running and the state
#' it was left in are binary, and reading them as text gives a run of
#' punctuation that says nothing. 65535 is the padding a writer uses to bring
#' the chunk to an even length, and is not a field at all.
#'
#' The Song Meter Mini Bat writes two more, 34 and 35, which neither the
#' reference implementation nor Wildlife Acoustics names. Each is 128 bytes,
#' mostly numbers, and read as text they give runs of control characters, with
#' the serial number of the device among those of 34. What can be recognised in
#' 34, the serial number, the position and the sample rate, the same device
#' writes as GUANO, where it is documented, so nothing is lost by passing them
#' over.
#' @keywords internal
#' @noRd
.WAMD_BINARY_IDS <- c(13L, 16L, 17L, 34L, 35L, 65535L)

#' Read a wamd timestamp
#'
#' Written as a date and time, optionally with a fraction of a second, and
#' optionally followed by the offset from UTC that the device was set to. A
#' time with no offset is read in the timezone of the session, as there is
#' nothing in the file to say which it was.
#'
#' @param text The field as it is written
#' @return A POSIXct, or the text where it is not a time this understands
#' @keywords internal
#' @noRd
.wamdTimestamp <- function(text) {
  if (!nzchar(text)) {
    return(text)
  }
  # ISO 8601 puts a colon in a numeric UTC offset, which R will not read.
  wanted <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", text)
  wanted <- sub("^([0-9]{4}-[0-9]{2}-[0-9]{2})[[:space:]]", "\\1T", wanted)
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS"
  )
  for (format in formats) {
    tz <- if (grepl("Z$|%z", format)) "UTC" else ""
    parsed <- as.POSIXct(wanted, format=format, tz=tz)
    if (!is.na(parsed)) {
      return(parsed)
    }
  }
  warning("Could not read the wamd timestamp '", text, "', keeping it as text.")
  return(text)
}

#' Read one coordinate of a wamd position
#'
#' Written either as a number followed by the letter of its hemisphere, or, by
#' some devices, as a signed number with no letter at all.
#'
#' @param text The coordinate as it is written
#' @param negative The hemisphere letter that makes the coordinate negative
#' @return A number, or NA where it could not be read
#' @keywords internal
#' @noRd
.wamdCoordinate <- function(text, negative) {
  letter <- toupper(substr(text, nchar(text), nchar(text)))
  value <- suppressWarnings(as.numeric(sub("[NSEWnsew]$", "", text)))
  if (is.na(value)) {
    return(NA_real_)
  }
  if (letter == negative) {
    return(-abs(value))
  }
  return(value)
}

#' Read a wamd position
#'
#' Written as the datum the position is given in, then a latitude, a longitude
#' and, where the device recorded one, an altitude in metres.
#'
#' @param text The field as it is written
#' @return A named numeric vector, or the text where it is not a position this
#'   understands
#' @keywords internal
#' @noRd
.wamdPosition <- function(text) {
  parts <- trimws(strsplit(text, ",", fixed=TRUE)[[1]])
  if (length(parts) < 3L) {
    return(text)
  }
  latitude <- .wamdCoordinate(parts[2], "S")
  longitude <- .wamdCoordinate(parts[3], "W")
  if (is.na(latitude) || is.na(longitude)) {
    warning("Could not read the wamd position '", text, "', keeping it as text.")
    return(text)
  }
  position <- c(latitude=latitude, longitude=longitude)
  if (length(parts) >= 4L) {
    altitude <- suppressWarnings(as.numeric(sub("[Mm]$", "", parts[4])))
    if (!is.na(altitude)) {
      position <- c(position, altitude=altitude)
    }
  }
  return(position)
}

#' Read one field of a wamd chunk as the type that field holds
#' @keywords internal
#' @noRd
.wamdValue <- function(name, bytes) {
  if (name %in% c("version", "time_expansion")) {
    return(as.integer(.rawToUInt(bytes)))
  }
  text <- .mdDecode(bytes, from="UTF-8")
  if (name == "timestamp") {
    return(.wamdTimestamp(text))
  }
  if (name == "gpsfirst") {
    return(.wamdPosition(text))
  }
  return(text)
}

#' Read the fields of a wamd chunk
#'
#' @param body The body of a wamd chunk
#' @return A list of the fields read and of the binary fields passed over
#' @keywords internal
#' @noRd
.wamdFields <- function(body) {
  fields <- list()
  binary <- numeric(0)
  pos <- 1L
  while (pos + 5L <= length(body)) {
    id <- .rawToUInt(body[pos:(pos + 1L)])
    size <- .rawToUInt(body[(pos + 2L):(pos + 5L)])
    if (size < 0 || pos + 6L + size - 1 > length(body)) {
      break
    }
    value <- if (size > 0) body[(pos + 6L):(pos + 6L + size - 1)] else raw(0)
    name <- unname(.WAMD_IDS[as.character(id)])
    if (is.na(name)) {
      # A field this package has no name for is kept under its number, so that
      # a device writing something new is not read as carrying nothing.
      name <- paste0("wamd:", id)
    }
    if (id %in% .WAMD_BINARY_IDS) {
      if (size > 0) {
        binary[name] <- size
      }
    } else if (size > 0) {
      read <- .wamdValue(name, value)
      if (length(read) > 0 && (!is.character(read) || nzchar(read))) {
        fields[[.mdUniqueName(fields, name)]] <- read
      }
    }
    pos <- pos + 6L + size
  }
  return(list(fields=fields, binary=binary))
}

#' Read Wildlife Acoustics metadata
#'
#' Reads the wamd chunk of a wave file, the metadata Wildlife Acoustics Song
#' Meter recorders write: the model of the device, its serial number, the
#' firmware it was running, the prefix of the filename it wrote, the time the
#' recording started with the offset from UTC the device was set to, the
#' position its receiver had, the microphone fitted and the identifications
#' their software has since made. A file holding no wamd chunk gives an empty
#' result rather than an error.
#'
#' The chunk names its fields by number rather than by name. The names used
#' here are those of the reference implementation published with the GUANO
#' specification; a field this package has no name for is reported under its
#' number, as `wamd:21`.
#'
#' The embedded voice note, the program the device was running and the state it
#' was left in are binary rather than text and are passed over, as are the two
#' unnamed fields 34 and 35 that the Song Meter Mini Bat writes. Their sizes
#' are kept on the result as its "binary" attribute, so that a file carrying
#' them can be told from one that does not.
#'
#' @param x Path to a wave file, a Wave object with wamd metadata attached, a
#'   raw vector holding a wave file, or a wamd object
#' @return A wamd object, which is a named list of fields
#' @export
#' @seealso [readMetadata()] reads this and every other metadata format a file
#'   carries at once. [guano()] reads the GUANO metadata that current Wildlife
#'   Acoustics firmware writes in place of this format, and [parseFilename()]
#'   reads the filename conventions of these recorders.
#' @examples
#' \dontrun{
#' m <- wamd("S4U09011_20210712_043000.wav")
#' m[["timestamp"]]
#' }
#'
wamd <- function(x) {
  return(.mdDispatch(
    x, "wamd",
    fromFile=function(file) .mdWamdObject(.mdWavChunk(file, "wamd")),
    fromBytes=function(bytes) .mdWamdObject(.mdWavChunkBytes(bytes, "wamd"))
  ))
}

#' Build a wamd object from the body of a wamd chunk
#' @keywords internal
#' @noRd
.mdWamdObject <- function(body) {
  if (is.null(body) || length(body) < 6L) {
    return(.mdObject(list(), "wamd"))
  }
  read <- .wamdFields(body)
  return(structure(.mdObject(read$fields, "wamd"), binary=read$binary))
}
