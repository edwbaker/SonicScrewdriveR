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
  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d"
  )
  for (format in formats) {
    tz <- if (grepl("Z$|%z", format)) "UTC" else ""
    parsed <- as.POSIXct(text, format=format, tz=tz)
    if (!is.na(parsed)) {
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
  tz <- attr(x, "tzone")
  if (!is.null(tz) && tz %in% c("UTC", "GMT")) {
    return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz="UTC"))
  }
  offset <- sub("([+-][0-9]{2})([0-9]{2})", "\\1:\\2", format(x, "%z"))
  return(paste0(format(x, "%Y-%m-%dT%H:%M:%S"), offset))
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
      parsed <- number(value)
      if (is.numeric(parsed)) as.integer(parsed) else parsed
    },
    position = number(strsplit(value, "[[:space:]]+")[[1]]),
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

  number <- function(x) format(x, scientific=FALSE, trim=TRUE)
  formatted <- switch(
    type,
    float = number(value),
    integer = number(as.integer(value)),
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
  if (any(grepl("\n", names(fields), fixed=TRUE))) {
    stop("A GUANO field name must not contain a newline.")
  }
  # The specification requires that GUANO|Version is the first field.
  order <- c(which(names(fields) == "GUANO|Version"), which(names(fields) != "GUANO|Version"))
  fields <- fields[order]

  values <- vapply(
    seq_along(fields),
    function(i) .guanoFormatValue(names(fields)[i], fields[[i]]),
    character(1)
  )
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
.guanoChunk <- function(x) {
  if (!inherits(x, "guano")) {
    stop("guano must be a guano object, see guano() and guanoCreate().")
  }
  body <- charToRaw(enc2utf8(as.character(x)))
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
  info <- tryCatch(.wavChunks(bytes), error=function(e) NULL)
  if (is.null(info)) {
    return(.guanoObject(list()))
  }
  chunk <- .wavFindChunk(info$chunks, "guan")
  if (is.null(chunk) || chunk$size < 1) {
    return(.guanoObject(list()))
  }
  return(guanoParse(.rawToText(bytes[chunk$start:(chunk$start + chunk$size - 1)])))
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
  return(.guanoFromBytes(readBin(x, "raw", n=file.size(x))))
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
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  if (is.null(output)) {
    output <- file
  }

  bytes <- readBin(file, "raw", n=file.size(file))
  info <- .wavChunks(bytes)
  existing <- .wavFindChunk(info$chunks, "guan")
  chunk <- .guanoChunk(x)

  if (is.null(existing)) {
    head <- bytes
    tail <- raw(0)
  } else {
    head <- bytes[seq_len(existing$start - 9L)]
    after <- existing$start + existing$size + (existing$size %% 2)
    tail <- if (after <= length(bytes)) bytes[after:length(bytes)] else raw(0)
  }

  head[5:8] <- .uintToRaw(length(head) + length(chunk) + length(tail) - 8, 4L)

  con <- file(output, "wb")
  on.exit(close(con))
  writeBin(head, con)
  writeBin(chunk, con)
  if (length(tail) > 0L) {
    writeBin(tail, con)
  }
  return(invisible(output))
}
