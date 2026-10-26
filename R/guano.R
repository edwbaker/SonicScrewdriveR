# GUANO (Grand Unified Acoustic Notation Ontology) metadata, version 1.0.
#
# GUANO is a metadata format for bioacoustic recordings, held in WAV files as a
# "guan" chunk of UTF-8 text. Each line is a "Key:Value" field, where the key
# may carry a namespace, as in "OAD|Battery Voltage". Fields a reader does not
# know are kept as written, as the specification requires.

#' Data types of the fields defined by the GUANO specification
#'
#' Any field not listed here, which includes every namespaced field, is read and
#' written as the string it appears as in the file.
#' @keywords internal
#' @noRd
.guanoTypes <- list(
  "GUANO|Version" = "string",
  "Filter HP" = "float",
  "Filter LP" = "float",
  "Firmware Version" = "string",
  "Hardware Version" = "string",
  "Humidity" = "float",
  "Length" = "float",
  "Loc Accuracy" = "float",
  "Loc Elevation" = "float",
  "Loc Position" = "position",
  "Make" = "string",
  "Model" = "string",
  "Note" = "multiline",
  "Original Filename" = "string",
  "Samplerate" = "integer",
  "Serial" = "string",
  "Species Auto ID" = "list",
  "Species Manual ID" = "list",
  "Tags" = "list",
  "TE" = "integer",
  "Temperature Ext" = "float",
  "Temperature Int" = "float",
  "Timestamp" = "datetime"
)

#' Build a guano object from a list of fields
#' @noRd
.guanoObject <- function(fields) {
  structure(fields, class="guano")
}

#' Read a GUANO timestamp
#'
#' The specification allows a date, a time, a local date and time, a UTC date
#' and time, or a date and time with a UTC offset.
#' @noRd
.guanoDatetime <- function(value) {
  # A time on its own has no date to attach it to, so it is kept as text.
  if (grepl("^[0-9]{2}:[0-9]{2}", value)) {
    return(value)
  }
  # R does not accept the colon that ISO 8601 puts in a numeric UTC offset.
  text <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
  # Every form is read in UTC, including those that name no zone: reading those
  # in the zone of the machine would make the moment a recording was made, and
  # what is written back, depend on where the file was opened, and shift it at
  # a daylight saving change. The form a value was written in is kept, so that
  # one naming no zone is not given one it never had.
  # The shape of each form is checked before it is read, as as.POSIXct() is
  # content with a value that merely begins with one: without this, a time of
  # 25:00:00 would be read as the date alone and the hour quietly dropped,
  # rather than kept as the text it is.
  clock <- "[0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?"
  forms <- list(
    list(shape=paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", clock, "[+-][0-9]{4}$"),
         format="%Y-%m-%dT%H:%M:%OS%z", zone="utc"),
    list(shape=paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", clock, "Z$"),
         format="%Y-%m-%dT%H:%M:%OSZ", zone="utc"),
    list(shape=paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", clock, "$"),
         format="%Y-%m-%dT%H:%M:%OS", zone="local"),
    list(shape="^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
         format="%Y-%m-%d", zone="date")
  )
  for (form in forms) {
    if (!grepl(form$shape, text)) {
      next
    }
    parsed <- as.POSIXct(text, format=form$format, tz="UTC")
    if (!is.na(parsed)) {
      if (form$zone != "utc") {
        attr(parsed, "guanoZone") <- form$zone
      }
      return(parsed)
    }
  }
  warning("Could not read the GUANO timestamp '", value, "', keeping it as text.")
  return(value)
}

#' Write a GUANO timestamp
#' @noRd
.guanoFormatDatetime <- function(x) {
  if (!inherits(x, "POSIXt")) {
    return(as.character(x))
  }
  # The specification allows a fractional second, and the parser reads one, so
  # dropping it here would move the start of a recording whenever metadata was
  # read and written back. A whole second is still written as one.
  #
  # The fraction is built from microseconds rather than trimmed out of what
  # format() gives, which would both leave the floating point nearest the value
  # written out in full and, were the zeros it dropped not anchored to the end,
  # take them from the middle of the fraction and change the time.
  seconds <- as.numeric(x)
  whole <- floor(seconds)
  micro <- round((seconds - whole) * 1e6)
  if (micro >= 1e6) {
    whole <- whole + 1
    micro <- 0
  }
  fraction <- if (micro == 0) "" else sub("0+$", "", sprintf(".%06d", micro))

  # A POSIXlt carries its zone as three strings, the zone and the names of its
  # standard and summer time, so only the first of them names the zone and
  # using the vector whole would raise an error rather than compare.
  tz <- attr(x, "tzone")
  tz <- if (length(tz) == 0L) "" else tz[1]
  at <- as.POSIXct(whole, origin="1970-01-01", tz=tz)

  # A value read from a field that named no zone is written back as it was
  # read, rather than given the zone of the machine it was read on.
  zone <- attr(x, "guanoZone")
  if (identical(zone, "date")) {
    return(format(at, "%Y-%m-%d", tz="UTC"))
  }
  if (identical(zone, "local")) {
    return(paste0(format(at, "%Y-%m-%dT%H:%M:%S", tz="UTC"), fraction))
  }

  if (tz %in% c("UTC", "GMT")) {
    return(paste0(format(at, "%Y-%m-%dT%H:%M:%S", tz="UTC"), fraction, "Z"))
  }
  offset <- sub("([+-][0-9]{2})([0-9]{2})", "\\1:\\2", format(at, "%z"))
  return(paste0(format(at, "%Y-%m-%dT%H:%M:%S"), fraction, offset))
}

#' Read a field value according to its defined type
#' @noRd
.guanoCoerce <- function(key, value) {
  type <- .guanoTypes[[key]]
  if (is.null(type)) {
    return(value)
  }
  number <- function(x) {
    parsed <- suppressWarnings(as.numeric(x))
    if (any(is.na(parsed))) {
      warning("Could not read the GUANO field '", key, "' as a number, keeping it as text.")
      return(value)
    }
    return(parsed)
  }
  switch(
    type,
    float = number(value),
    integer = {
      # A whole number the field is meant to hold, and one R can hold as a
      # whole number. Anything else, a fraction or a number too large, would
      # come back cut down or as nothing at all, so it is kept as the text it
      # is: editing one field should not quietly destroy another.
      parsed <- number(value)
      if (!is.numeric(parsed)) {
        parsed
      } else if (length(parsed) != 1 || !is.finite(parsed) ||
                 parsed != round(parsed) ||
                 abs(parsed) > .Machine$integer.max) {
        warning(
          "The GUANO field '", key, "' is not a whole number that can be ",
          "held as one, keeping it as text."
        )
        value
      } else {
        as.integer(parsed)
      }
    },
    position = {
      # The specification gives a position as a latitude and a longitude.
      # Anything else is not one, and reading it as numbers would both accept
      # it and alter it: a third number written back as 99.0 where it was 99.
      parsed <- number(strsplit(trimws(value), "[[:space:]]+")[[1]])
      if (is.numeric(parsed) && length(parsed) != 2) {
        warning(
          "The GUANO field '", key, "' is not a latitude and a longitude, ",
          "keeping it as text."
        )
        value
      } else {
        parsed
      }
    },
    list = trimws(strsplit(value, ",", fixed=TRUE)[[1]]),
    multiline = gsub("\\n", "\n", value, fixed=TRUE),
    datetime = .guanoDatetime(value),
    value
  )
}

#' Write a field value according to its defined type
#' @noRd
.guanoFormatValue <- function(key, value) {
  type <- .guanoTypes[[key]]
  if (is.null(type)) {
    return(paste(as.character(value), collapse=","))
  }
  # A value that could not be read as the type its field is defined to have was
  # kept as the text it appeared as, and is written back as that text.
  if (type %in% c("float", "integer", "position") && !is.numeric(value)) {
    return(paste(as.character(value), collapse=" "))
  }

  # format() otherwise writes as many digits as the session is set to display,
  # so reading a position of six decimal places and writing it back would move
  # it, by a distance depending on options(digits). Fifteen significant digits
  # carry a double through a round trip without printing the noise that the
  # seventeen needed to be exact would.
  # The decimal mark is given as well, since format() otherwise follows
  # options(OutDec), and a session set to a comma would write a number that
  # guanoParse() could not read back as one.
  number <- function(x) {
    format(x, scientific=FALSE, trim=TRUE, digits=15, decimal.mark=".")
  }
  formatted <- switch(
    type,
    float = number(value),
    integer = {
      # Written as the whole number it is, rather than narrowed to what an R
      # integer holds, which turns anything larger into nothing.
      if (!is.finite(value) || value != round(value)) {
        stop(
          "The GUANO field '", key, "' must be a whole number, not ",
          format(value, scientific=FALSE), "."
        )
      }
      number(value)
    },
    position = paste(number(value), collapse=" "),
    list = paste(as.character(value), collapse=","),
    multiline = gsub("\n", "\\n", as.character(value), fixed=TRUE),
    datetime = .guanoFormatDatetime(value),
    as.character(value)
  )
  return(paste(formatted, collapse=" "))
}

#' Parse a block of GUANO metadata
#'
#' Reads the text of a GUANO metadata block into a guano object. Fields the
#' specification defines are converted to the type it gives them, and any other
#' field, which includes every namespaced field, is kept as the string it
#' appears as in the file.
#'
#' @param text A string holding a GUANO metadata block
#' @return A guano object, which is a named list of fields
#' @export
#' @references
#'   Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
#'   \url{https://github.com/riggsd/guano-spec}
#' @examples
#' guanoParse("GUANO|Version:1.0\nMake:Open Acoustic Devices\nSamplerate:48000")
#'
guanoParse <- function(text) {
  if (length(text) != 1L || !is.character(text)) {
    stop("text must be a single string.")
  }
  fields <- list()
  for (line in strsplit(text, "\n", fixed=TRUE)[[1]]) {
    if (!nzchar(trimws(line))) {
      next
    }
    at <- regexpr(":", line, fixed=TRUE)
    if (at < 1L) {
      warning("Ignoring the GUANO line '", trimws(line), "', which has no value.")
      next
    }
    key <- trimws(substr(line, 1L, at - 1L))
    value <- trimws(substr(line, at + 1L, nchar(line)))
    if (!nzchar(key)) {
      next
    }
    if (key %in% names(fields)) {
      warning("The GUANO field '", key, "' appears more than once, keeping the last.")
    }
    fields[[key]] <- .guanoCoerce(key, value)
  }
  return(.guanoObject(fields))
}

#' Create GUANO metadata
#'
#' Builds a guano object from named fields. Field names follow the GUANO
#' specification, so a name may carry a namespace, as in "OAD|Battery Voltage",
#' and the specification requires that any field it does not define is
#' namespaced.
#'
#' @param ... Named fields. A name containing a space or a vertical bar must be
#'   quoted.
#' @param version The GUANO version to record
#' @return A guano object, which is a named list of fields
#' @export
#' @references
#'   Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
#'   \url{https://github.com/riggsd/guano-spec}
#' @examples
#' guanoCreate(
#'   Make = "Open Acoustic Devices",
#'   Model = "AudioMoth",
#'   Samplerate = 48000,
#'   "Loc Position" = c(51.4964, -0.1224),
#'   "Species Manual ID" = c("Pipistrellus pipistrellus", "Myotis daubentonii")
#' )
#'
guanoCreate <- function(..., version="1.0") {
  version <- .oneValue(version, "version")
  fields <- list(...)
  if (length(fields) > 0L && (is.null(names(fields)) || any(!nzchar(names(fields))))) {
    stop("Every field must be named.")
  }
  if (anyDuplicated(names(fields))) {
    stop("A GUANO field name must not be used more than once.")
  }
  return(.guanoObject(c(list("GUANO|Version"=version), fields[names(fields) != "GUANO|Version"])))
}

#' @export
as.character.guano <- function(x, ...) {
  fields <- unclass(x)
  if (length(fields) == 0L) {
    fields <- list("GUANO|Version"="1.0")
  }
  if (is.null(fields[["GUANO|Version"]])) {
    fields <- c(list("GUANO|Version"="1.0"), fields)
  }
  if (anyDuplicated(names(fields))) {
    stop("A GUANO field name must not be used more than once.")
  }
  # A field is a line, and a name ends at its first colon, so a newline in
  # either part or a colon in a name would be read back as something other
  # than what was written: a value holding a newline would become fields of
  # its own, and could rewrite the ones around it.
  if (any(grepl("[\r\n]", names(fields)))) {
    stop("A GUANO field name must not contain a newline.")
  }
  if (any(grepl(":", names(fields), fixed=TRUE))) {
    stop("A GUANO field name must not contain a colon, which ends the name.")
  }
  # The specification requires that GUANO|Version is the first field.
  order <- c(which(names(fields) == "GUANO|Version"), which(names(fields) != "GUANO|Version"))
  fields <- fields[order]

  values <- vapply(
    seq_along(fields),
    function(i) .guanoFormatValue(names(fields)[i], fields[[i]]),
    character(1)
  )
  # Note escapes the newlines it is allowed to hold, so anything left here
  # would be read back as a field of its own rather than as part of a value.
  if (any(grepl("[\r\n]", values))) {
    stop(
      "A GUANO value must not contain a newline: ",
      names(fields)[grepl("[\r\n]", values)][1]
    )
  }
  return(paste(paste0(names(fields), ":", values), collapse="\n"))
}

#' @export
format.guano <- function(x, ...) {
  as.character(x, ...)
}

#' @export
print.guano <- function(x, ...) {
  fields <- unclass(x)
  if (length(fields) == 0L) {
    cat("<no GUANO metadata>\n")
    return(invisible(x))
  }
  width <- max(nchar(names(fields)))
  for (key in names(fields)) {
    cat(formatC(key, width=width, flag="-"), " ", .guanoFormatValue(key, fields[[key]]), "\n", sep="")
  }
  return(invisible(x))
}

#' @export
`[.guano` <- function(x, ...) {
  .guanoObject(NextMethod())
}

#' Build the guan chunk holding a GUANO metadata block
#'
#' The block is padded with a space to an even number of bytes, as RIFF chunks
#' must be word aligned and the specification allows whitespace padding.
#' @noRd
.guanoChunk <- function(x, size=NULL) {
  if (!inherits(x, "guano")) {
    stop("guano must be a guano object, see guano() and guanoCreate().")
  }
  body <- charToRaw(enc2utf8(as.character(x)))
  if (!is.null(size)) {
    if (size < length(body)) {
      stop("The metadata does not fit in the ", size, " bytes asked for.")
    }
    body <- c(body, charToRaw(strrep(" ", size - length(body))))
  }
  if (length(body) %% 2L == 1L) {
    body <- c(body, charToRaw(" "))
  }
  return(c(charToRaw("guan"), .uintToRaw(length(body), 4L), body))
}

#' Read the text of a chunk, dropping the padding bytes
#' @noRd
.rawToText <- function(bytes) {
  bytes <- bytes[as.integer(bytes) != 0L]
  if (length(bytes) == 0L) {
    return("")
  }
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  return(text)
}

#' Read the GUANO metadata of a WAV file held as raw bytes
#' @noRd
.guanoFromBytes <- function(bytes) {
  # Only a file this package cannot make sense of as a wave file holds no
  # metadata as far as this is concerned. Anything else is raised, as it is
  # when the metadata is read from a file rather than from bytes in hand.
  info <- tryCatch(.wavChunks(bytes), unsupportedWav=function(cond) NULL)
  if (is.null(info)) {
    return(.guanoObject(list()))
  }
  chunk <- .wavFindChunk(info$chunks, "guan")
  if (is.null(chunk) || chunk$size < 1) {
    return(.guanoObject(list()))
  }
  return(guanoParse(.rawToText(bytes[chunk$start:(chunk$start + chunk$size - 1)])))
}

#' Read the GUANO metadata of a WAV file
#'
#' Only the metadata chunk is read, rather than the whole recording, so that
#' asking a long recording for a field needs no more memory than the metadata
#' itself.
#' @noRd
.guanoFromFile <- function(file) {
  # A file that is not one this package can read holds no metadata, and says
  # so. A file it cannot open or read at all is a different matter, and saying
  # the metadata is absent would report a file that could not be read as one
  # carrying nothing.
  body <- tryCatch(.wavChunkFromFile(file, "guan"), unsupportedWav=function(cond) NULL)
  if (is.null(body)) {
    return(.guanoObject(list()))
  }
  return(guanoParse(.rawToText(body)))
}

#' Read GUANO metadata
#'
#' Reads the GUANO metadata of a WAV file, or of a Wave object that carries it.
#' GUANO is the metadata format used by many bioacoustic recorders, including
#' AudioMoth devices running current firmware. A file holding no GUANO metadata
#' gives an empty result rather than an error.
#'
#' @param x Path to a WAV file, a Wave object with GUANO metadata attached, or a
#'   raw vector holding a whole WAV file
#' @return A guano object, which is a named list of fields
#' @export
#' @references
#'   Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
#'   \url{https://github.com/riggsd/guano-spec}
#' @examples
#' \dontrun{
#' m <- guano("20201103_112430.WAV")
#' m[["Loc Position"]]
#' }
#'
guano <- function(x) {
  if (inherits(x, "guano")) {
    return(x)
  }
  if (inherits(x, "Wave") || inherits(x, "WaveMC")) {
    metadata <- attr(x, "guano")
    return(if (is.null(metadata)) .guanoObject(list()) else metadata)
  }
  if (is.raw(x)) {
    return(.guanoFromBytes(x))
  }
  if (!is.character(x) || length(x) != 1L) {
    stop("x must be a file path, a Wave object, or a raw vector.")
  }
  if (!file.exists(x)) {
    stop("File does not exist: ", x)
  }
  return(.guanoFromFile(x))
}

#' Attach GUANO metadata to a Wave object
#'
#' Wave objects have no place to keep file metadata, so GUANO metadata read from
#' a file is kept as an attribute, which [guano()] reads back. Writing the wave
#' to a file does not carry the metadata with it: use [guanoWrite()] on the
#' written file, or pass the metadata to [audiomothCompress()].
#'
#' @param x A Wave or WaveMC object
#' @param value A guano object, or NULL to remove the metadata
#' @return The object with the metadata attached
#' @export
#' @examples
#' \dontrun{
#' guano(wave) <- guanoCreate(Make="Open Acoustic Devices", Model="AudioMoth")
#' }
#'
`guano<-` <- function(x, value) {
  if (!inherits(x, "Wave") && !inherits(x, "WaveMC")) {
    stop("GUANO metadata can only be attached to a Wave or WaveMC object.")
  }
  if (!is.null(value) && !inherits(value, "guano")) {
    stop("value must be a guano object, see guano() and guanoCreate().")
  }
  attr(x, "guano") <- value
  return(x)
}

#' Write GUANO metadata to a WAV file
#'
#' Replaces the GUANO metadata of a WAV file, adding a guan chunk if the file
#' has none. Only the metadata chunk and the sizes in the file header are
#' changed, so the audio data and every other chunk are left as they were.
#'
#' @param x A guano object, from [guano()] or [guanoCreate()]
#' @param file Path to the WAV file to read
#' @param output Path to write to. If NULL (the default) `file` is rewritten in
#'   place.
#' @return The path to the written file (invisibly)
#' @export
#' @references
#'   Riggs, D. GUANO: Grand Unified Acoustic Notation Ontology.
#'   \url{https://github.com/riggsd/guano-spec}
#' @examples
#' \dontrun{
#' m <- guano("20201103_112430.WAV")
#' m[["Species Manual ID"]] <- "Pipistrellus pipistrellus"
#' guanoWrite(m, "20201103_112430.WAV")
#' }
#'
guanoWrite <- function(x, file, output=NULL) {
  if (!inherits(x, "guano")) {
    stop("x must be a guano object, see guano() and guanoCreate().")
  }
  file <- .oneValue(file, "file")
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  output <- if (is.null(output)) file else .oneValue(output, "output")

  bytes <- readBin(file, "raw", n=file.size(file))
  info <- .wavChunks(bytes)
  existing <- .wavFindChunk(info$chunks, "guan")
  chunk <- .guanoChunk(x)

  # A recording cut short declares more audio than it holds. Metadata written
  # after it would fall inside the audio the file claims to have, where no
  # reader, this one included, would find it. The size is corrected to the
  # audio that is actually there, which is the only size that was ever true.
  data <- .wavFindChunk(info$chunks, "data")

  # Where the metadata is held before the audio, replacing it with a chunk of a
  # different size moves the audio within the file. An AudioMoth triggered
  # recording marks its omitted periods with blocks found by their position in
  # the file, counted in 512 byte blocks from the start of it, so moving the
  # audio by anything else loses them: the recording still holds every sample
  # it did, but expanding it no longer restores the periods left out, and the
  # timing of everything after the first of them is wrong. Keeping the size
  # difference to whole blocks leaves them where they were. The specification
  # allows the padding this takes.
  if (!is.null(existing) && !is.null(data) && existing$start < data$start) {
    held <- 8 + existing$size + (existing$size %% 2)
    needed <- held + .AM_BLOCK_SIZE * ceiling((length(chunk) - held) / .AM_BLOCK_SIZE)
    if (needed != length(chunk)) {
      chunk <- .guanoChunk(x, size=needed - 8)
    }
  }

  if (!is.null(data) && isTRUE(data$truncated)) {
    warning(
      "DATA chunk runs past the end of the file, which may be truncated. ",
      "Its size has been corrected to the audio the file holds."
    )
    bytes[(data$start - 4L):(data$start - 1L)] <- .uintToRaw(data$size, 4L)
  }

  if (is.null(existing)) {
    head <- bytes
    tail <- raw(0)
  } else {
    head <- bytes[seq_len(existing$start - 9L)]
    after <- existing$start + existing$size + (existing$size %% 2)
    tail <- if (after <= length(bytes)) bytes[after:length(bytes)] else raw(0)
  }

  # A RIFF chunk of an odd length is followed by a padding byte, so a recording
  # holding an odd number of audio bytes needs one before anything is written
  # after it. Without it the metadata would start a byte early and be read as a
  # chunk of some other name.
  if (length(head) %% 2L == 1L) {
    head <- c(head, raw(1))
  }

  # length() gives a whole number, and a recording of 2GB or more is longer
  # than their sum can be counted in, so the size is added up in doubles.
  head[5:8] <- .uintToRaw(
    as.numeric(length(head)) + length(chunk) + length(tail) - 8, 4L
  )

  # Written beside the recording and put in its place once whole, so that a
  # write which fails part way through cannot lose a recording that was only
  # having one field of its metadata changed.
  return(.writeAtomically(output, function(con) {
    writeBin(head, con)
    writeBin(chunk, con)
    if (length(tail) > 0L) {
      writeBin(tail, con)
    }
  }))
}
