#' A set of recordings
#'
#' A `RecordingSet` is a data frame with one row for each recording, holding
#' what is known of each recording without reading it. It is what an analysis
#' of many recordings is run over, wherever they come from. Each place that
#' recordings come from describes them in its own way, and is mapped onto the
#' same columns here, so that sets from different places can be combined with
#' `rbind()`.
#'
#' Every set has these columns, in this order, with `NA` where a value is not
#' known:
#'
#' * `uri`: what the recording is, unique within the set and never `NA`. A
#'   recording held by audioBlast has the address audioBlast gives it,
#'   `https://api.audioblast.org/recording/{source}/{id}`, and a file the
#'   `file://` address of its path.
#' * `source`: the collection the recording is from, e.g. "xeno-canto".
#' * `location`: where the audio can be read from, a path or a URL.
#' * `hash`: the SHA-256 hash of the file, in lower case.
#' * `duration`: in seconds.
#' * `sample_rate`: in Hz.
#' * `channels`: the number of channels.
#' * `codec`: the format the audio is in, e.g. "mp3" or "pcm_s16le".
#' * `status`: how measuring the file went, "ok", "missing" or "unreadable", or
#'   `NA` where it has not been measured.
#' * `start`: when the recording began, in UTC. It is given only where the time
#'   zone the recording was made in is known, and is never guessed.
#' * `tz`: the time zone the recording was made in.
#' * `lat`, `lon`: where the recording was made, in decimal degrees.
#' * `taxon`: what the recording is of.
#' * `author`, `license`, `rights_holder`: who made the recording, the URL of
#'   its licence, and who holds the rights in it. A licence that is `NA` is one
#'   that is not known, not one that leaves the recording free to use.
#'
#' Any other columns a source gives follow these. An empty string in one of
#' the columns above is taken to be a value that is not known, and is `NA`.
#'
#' Several recordings can be of one file: the Animal Sound Archive lists a
#' recording of several species once for each of them. Recordings that share a
#' hash or a location are counted as one file.
#'
#' Making a set from files checks that they exist but reads nothing from them,
#' so their measurements are `NA`. [audioblast()] with `output="RecordingSet"`
#' gives a set of recordings held by audioBlast, with what audioBlast has
#' measured of their files.
#'
#' Subsetting a set keeps it a set while the `uri` and `location` columns
#' remain, and gives a plain data frame once either is dropped. Sets are
#' combined with `rbind()`, with a set as the first argument: a column that only
#' some of the sets have is `NA` in the rows of the others.
#'
#' @param x Paths of audio files, or a data frame with at least the columns
#'   `uri` and `location`.
#' @param source For files, the collection they are from: one value for all of
#'   them, or one for each.
#' @param ... Not used.
#' @return A `RecordingSet`.
#' @aliases RecordingSet
#' @seealso [audioblast()] for a set of recordings held by audioBlast.
#' @export
#' @examples
#' f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
#' recordingSet(f, source="AudioMoth")
#'
#' \dontrun{
#' rs <- audioblast("data", "recordings", source="bio.acousti.ca",
#'                  max_pages=1, output="RecordingSet")
#' rbind(rs, recordingSet(f))
#' }
recordingSet <- function(x, ...) {
  UseMethod("recordingSet")
}

#' @rdname recordingSet
#' @export
recordingSet.default <- function(x, ...) {
  stop("A RecordingSet is made from paths of audio files or from a data frame, not from ",
       class(x)[1], ".")
}

#' @rdname recordingSet
#' @export
recordingSet.character <- function(x, source=NA_character_, ...) {
  if (anyNA(x)) {
    stop("The path of a file cannot be NA.")
  }
  if (!length(source) %in% c(1L, length(x))) {
    stop("source must be a single value, or one value for each file.")
  }
  absent <- !file.exists(x) | dir.exists(x)
  if (any(absent)) {
    stop(
      sum(absent), if (sum(absent) == 1) " file does" else " files do",
      " not exist: ", paste(utils::head(x[absent], 3), collapse=", "),
      if (sum(absent) > 3) ", ...", "."
    )
  }
  path <- normalizePath(x, winslash="/", mustWork=TRUE)
  return(recordingSet(data.frame(
    uri = .fileUri(path),
    source = rep_len(as.character(source), length(x)),
    location = path,
    stringsAsFactors = FALSE
  )))
}

#' @rdname recordingSet
#' @export
recordingSet.data.frame <- function(x, ...) {
  for (col in c("uri", "location")) {
    if (!col %in% names(x)) {
      stop("A RecordingSet needs a ", col, " column.")
    }
  }
  x <- as.data.frame(x, stringsAsFactors=FALSE)
  core <- names(.recordingSetColumns)
  for (col in core) {
    template <- .recordingSetColumns[[col]]
    x[[col]] <- if (col %in% names(x)) {
      .recordingSetColumn(x[[col]], template, col)
    } else {
      template[rep(NA_integer_, nrow(x))]
    }
  }
  if (anyNA(x$uri)) {
    stop("Every recording needs a uri, and ", sum(is.na(x$uri)), " have none.")
  }
  repeated <- unique(x$uri[duplicated(x$uri)])
  if (length(repeated) > 0) {
    stop(
      length(repeated), if (length(repeated) == 1) " uri appears" else " uris appear",
      " more than once, e.g. ", repeated[1], ". A recording can only be in a ",
      "RecordingSet once."
    )
  }
  x <- x[c(core, setdiff(names(x), core))]
  rownames(x) <- NULL
  class(x) <- c("RecordingSet", "data.frame")
  return(x)
}

# The columns every RecordingSet has, in order, each of the type given here
.recordingSetColumns <- list(
  uri = character(0),
  source = character(0),
  location = character(0),
  hash = character(0),
  duration = numeric(0),
  sample_rate = numeric(0),
  channels = integer(0),
  codec = character(0),
  status = character(0),
  start = .POSIXct(numeric(0), tz="UTC"),
  tz = character(0),
  lat = numeric(0),
  lon = numeric(0),
  taxon = character(0),
  author = character(0),
  license = character(0),
  rights_holder = character(0)
)

#' One column of a RecordingSet, as the type the set holds it as
#'
#' @param value The column as it was given.
#' @param template A vector of no length, of the type the column is held as.
#' @param col The name of the column, for the errors.
#' @return The column, converted.
#' @keywords internal
#' @noRd
.recordingSetColumn <- function(value, template, col) {
  if (inherits(template, "POSIXct")) {
    if (all(is.na(value))) {
      return(template[rep(NA_integer_, length(value))])
    }
    if (!inherits(value, "POSIXct")) {
      stop("The ", col, " column must hold dates and times (POSIXct), not ",
           class(value)[1], ".")
    }
    #The same instants, written in UTC
    attr(value, "tzone") <- "UTC"
    return(value)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.character(value)) {
    value[.isBlank(value)] <- NA
  }
  if (is.character(template)) {
    value <- as.character(value)
    if (col == "hash") {
      value <- tolower(value)
    }
    return(value)
  }
  number <- suppressWarnings(as.numeric(value))
  bad <- is.na(number) & !is.na(value)
  if (is.integer(template)) {
    bad <- bad | (!is.na(number) & number != round(number))
  }
  if (any(bad)) {
    stop("The ", col, " column holds values that are not ",
         if (is.integer(template)) "whole numbers" else "numbers",
         ", e.g. ", value[bad][1], ".")
  }
  if (is.integer(template)) {
    return(as.integer(number))
  }
  return(number)
}

#' Which strings hold nothing but white space
#'
#' trimws() runs two regular expressions over every string, which took most of
#' the half a minute that making a set of a million recordings took. One that
#' stops at the first character that is not a space does the same.
#' @param x A character vector.
#' @return A logical vector, FALSE for NA.
#' @keywords internal
#' @noRd
.isBlank <- function(x) {
  return(!is.na(x) & !grepl("[^[:space:]]", x, perl=TRUE))
}

#' The file:// address of a path
#'
#' Each part of the path is percent-encoded, a percent sign included, so that a
#' file named with a space, a hash or a percent sign has an address that names
#' it and no other.
#' @param path Absolute paths, with forward slashes.
#' @return The address of each path.
#' @keywords internal
#' @noRd
.fileUri <- function(path) {
  vapply(path, function(p) {
    #A network path, //server/share, names its server where a local path does not
    network <- startsWith(p, "//")
    parts <- strsplit(sub("^/+", "", p), "/", fixed=TRUE)[[1]]
    drive <- grepl("^[A-Za-z]:$", parts) & seq_along(parts) == 1
    parts[!drive] <- utils::URLencode(parts[!drive], reserved=TRUE, repeated=TRUE)
    paste0(if (network) "file://" else "file:///", paste(parts, collapse="/"))
  }, character(1), USE.NAMES=FALSE)
}

#' Which recordings of a set are of the same file
#'
#' Recordings are of one file where they share a hash or a location, and so are
#' recordings joined through others: one sharing a hash with a second that
#' shares a location with a third.
#' Each recording starts in a group of its own, and takes the lowest group of
#' the recordings that share its hash, then of those that share its location,
#' until no group changes. ave() did the same a group at a time, and took two
#' minutes over a set of 200,000 recordings.
#' @param x A RecordingSet.
#' @return An integer for each recording, the same for recordings of one file.
#' @keywords internal
#' @noRd
.recordingSetFiles <- function(x) {
  group <- seq_len(nrow(x))
  #Each key as a number, NA where it is not known
  keys <- lapply(list(x$hash, x$location), function(k) match(k, unique(k[!is.na(k)])))
  repeat {
    before <- group
    for (key in keys) {
      known <- which(!is.na(key))
      if (length(known) > 0) {
        #The lowest group of each key, found by sorting on the key then the group
        k <- key[known]
        o <- order(k, group[known])
        first <- o[!duplicated(k[o])]
        lowest <- integer(max(k))
        lowest[k[first]] <- group[known][first]
        group[known] <- lowest[k]
      }
    }
    if (identical(group, before)) {
      break
    }
  }
  return(match(group, unique(group)))
}

#' @export
print.RecordingSet <- function(x, n=6, ...) {
  recordings <- nrow(x)
  files <- length(unique(.recordingSetFiles(x)))
  sources <- length(unique(x$source[!is.na(x$source)]))
  cat(
    "<RecordingSet: ", recordings, if (recordings == 1) " recording" else " recordings",
    " of ", files, if (files == 1) " file" else " files",
    ", from ", sources, if (sources == 1) " source" else " sources", ">\n",
    sep=""
  )
  #A column taken out with $<- leaves the class in place
  shown <- intersect(c("uri", "source", "duration", "sample_rate", "channels", "status"), names(x))
  if (recordings > 0) {
    print.data.frame(as.data.frame(x)[seq_len(min(n, recordings)), shown], row.names=FALSE)
  }
  if (recordings > n) {
    cat("... and ", recordings - n, " more\n", sep="")
  }
  others <- setdiff(names(x), shown)
  cat(strwrap(paste0("Other columns: ", paste(others, collapse=", ")), exdent=2), sep="\n")
  return(invisible(x))
}

#' @export
`[.RecordingSet` <- function(x, ...) {
  out <- NextMethod()
  #A data frame without these is no longer a set of recordings
  if (is.data.frame(out) && !all(c("uri", "location") %in% names(out))) {
    class(out) <- setdiff(class(out), "RecordingSet")
  }
  return(out)
}

#' @export
rbind.RecordingSet <- function(..., deparse.level=1) {
  sets <- list(...)
  sets <- lapply(sets[!vapply(sets, is.null, logical(1))], recordingSet)
  #One NA of each column, of the type the first set that has it holds it as
  absent <- list()
  for (s in sets) {
    for (col in setdiff(names(s), names(absent))) {
      absent[[col]] <- .audioblast_na(s[[col]], 1L)
    }
  }
  sets <- lapply(sets, function(s) {
    s <- as.data.frame(s)
    for (col in setdiff(names(absent), names(s))) {
      s[[col]] <- .audioblast_na(absent[[col]], nrow(s))
    }
    s[names(absent)]
  })
  return(recordingSet(do.call(rbind, sets)))
}
