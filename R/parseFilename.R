#' Parse a filename
#'
#' Attempts to extract meaningful information from a filename, typically the
#' date and time a recording started.
#'
#' @details
#' ## Determining the format
#' It is sometimes impossible to accurately determine the format of
#' a filename, e.g. when an eight-digit 'AudioMoth HEX' only contains numbers it
#' could be confused with a YYYYMMDD format. If a list of filenames is given
#' and the "match" format is specified then an effort will be made to determine
#' the most likely format that applies to all filenames.
#'
#' A reading that could not be settled from the name says so: `ambiguous` is
#' TRUE and `alternatives` names every format the filename could be in, the one
#' that was picked included. Neither field is present where the format was not
#' in doubt. A name of nothing but digits is the case that matters, because a
#' collection that numbers its records gives names of exactly that shape: eight
#' digits are read as a date and ten as a count of seconds, and both read just
#' as well as a record number. [readMetadata()] therefore takes no date from
#' such a name, as a confident wrong date is worse than none, though it is
#' reported here for a caller that knows what its own files are named by.
#'
#' Asking for a format by name settles it: a format given in `format` is not a
#' guess and carries no alternatives. Finding one that fits every name in a
#' list, with `format = "match"`, does not settle it, since a list of record
#' numbers agrees with itself exactly as readily as a list of dates does.
#'
#' ## Timezones
#' Most supported formats write a wall clock (e.g. 20200609_191730) into the
#' filename, with no indication of the timezone the clock of the recorder was
#' set to. The `timezone` argument states which timezone that wall clock
#' belongs to, and defaults to UTC, which is what AudioMoth devices use unless
#' they are configured otherwise. The datetime returned is therefore the same
#' instant whatever timezone the machine running the analysis is set to.
#'
#' The 'AudioMoth HEX' and 'timestamp' formats are exceptions: these encode an
#' instant rather than a wall clock, so for them `timezone` only sets the
#' timezone the datetime is displayed in.
#'
#' ## Supported formats
#' * **AudioMoth** - The newer format for AudioMoth devices consists of a
#'   standard YYYYMMDD_HHMMSS.wav format. An eight character hexadecimal
#'   filename given this format is treated as 'AudioMoth HEX'.
#' * **AudioMoth HEX** - Older format for AudioMoth devices consisting of eight
#'   hexadecimal characters giving the number of seconds since
#'   1970-01-01 00:00:00 UTC. Conversion is handled by a call to the
#'   `audiomoth()` function from the `seewave` package
#'   \insertCite{seewave2008}{sonicscrewdriver}.
#' * **timestamp** - A standard date-time format. Uses the R standard origin of
#'   1970-01-01 00:00:00 UTC.
#' * **TSA** - The convention of the TSA corpus and the collections distributed
#'   with it, as `TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav`: a
#'   collection, the taxon or common name of the animal recorded with the
#'   accession number the collection gave it, and a hash of the file. Gives
#'   `collection`, `taxon`, `accession` and `hash`, and no datetime, as these
#'   names carry no time. Much of that corpus holds no embedded metadata at
#'   all, so for those recordings the name is the only record of what was
#'   recorded.
#'
#'   A `taxon` is read only where the name carries a scientific name, which is
#'   a capitalised genus and a lowercase specific epithet joined by an
#'   underscore or a hyphen. Part of the corpus is named by a German common
#'   name and a German word for the kind of call instead; no taxon is read from
#'   those, as reporting their first words as a genus and a species would
#'   invent one. A subspecies cannot be told from an accession number that
#'   happens to be a bare word, so the first two parts of a scientific name are
#'   taken as the name and the rest as the accession number.
#' * **Wildlife Acoustics SM2** - Can also be used for Wildlife Acoustics SM4
#'   devices. The filename is split by a call to `seewave::songmeter()`.
#' * **Wildlife Acoustics SM3** - The filename is split by a call to
#'   `seewave::songmeter()`.
#' * **YYYYMMDD_HHMMSS** - A standard date-time format.
#' * **YYYYMMDD** - A standard date format, the time is taken to be midnight.
#'
#' @param file A filename, or several filenames as either a list or a
#'  character vector (the shape returned by `list.files()`).
#' @param format Optionally force a given format (see Details). If NULL (default)
#'  an attempt is made to automatically detect the format for each file. If "match"
#'  and a list of filenames is given then an attempt will be made to find a format
#'  that matches all files. This may give incorrect results if the filename is
#'  ambiguous (see Details).
#' @param timezone The timezone the clock of the recorder was set to, defaults
#'  to "UTC" (see Details).
#' @return A list of file, type of match, datetime. If a list of filenames is
#'  given then a list of such lists is returned.
#' \cr\cr
#' It is possible to determine additional properties from some files, these will
#' be added to the list. Which properties those are depends on the format: a
#' Wildlife Acoustics name gives a model, a prefix, a microphone and a
#' deployment, a TSA name gives a collection, a taxon, an accession number and a
#' hash, and a reading the name left in doubt gives `ambiguous` and
#' `alternatives`. A format carrying no time, which TSA names do not, reports
#' `datetime` as NA rather than leaving it out.
#' @references
#'   \insertAllCited{}
#' @seealso [readMetadata()] reads a file's own embedded metadata and puts what
#'   its name says beside it.
#' @export
#' @examples
#' parseFilename("5E90A4D4.wav")
#' parseFilename("TSA-Porzana_parva_DIG_104_26_1-ee281ac7afd44e08.wav")$taxon
#'
#' # Eight digits could be a date or a record number, and the reading says so.
#' parseFilename("20240220.wav")$alternatives
#'
parseFilename <- function(file, format=NULL, timezone="UTC") {
  if (is.null(timezone)) {
    timezone <- "UTC"
  }
  if (is.character(file) && length(file) > 1) {
    # list.files() returns a character vector of filenames; treat it the same
    # as a list of filenames.
    file <- as.list(file)
  }
  if (!is.null(format) && identical(as.character(format), "match")) {
    # Matching a format across files is only meaningful for a list of files,
    # for a single file it is the same as detecting the format.
    format <- if (is(file, "list")) .matchFormat(file) else NULL
  }
  if (is(file, "list")) {
    return(lapply(file, parseFilename, format=format, timezone=timezone))
  }
  if (is.null(format)) {
    format <- .detectFormat(file)
    if (is.null(format)) {
      stop("Could not determine format of ", file)
    }
  }
  .validateChoice(format, .knownFileFormats(), msg=paste("Unknown format:", format))

  # Which other formats the name could have been in, where it was detected
  # rather than given. A format the caller named is not a guess and carries
  # none. The attributes come off once it has been read, so that the format
  # reported back is the plain string it names and nothing else.
  alternatives <- attr(format, "alternatives")
  format <- as.character(format)

  bn <- tools::file_path_sans_ext(basename(file))

  if (format == "TSA") {
    parts <- .tsaParts(bn)
    if (is.null(parts)) {
      stop("Could not read ", file, " as a TSA filename.")
    }
    # These names carry no time of any kind, so the field every other format
    # fills is reported as missing rather than left out, which would change the
    # shape of the result from one filename to the next.
    return(c(
      list(filename=file, match=format, datetime=NA),
      parts,
      .ambiguityOf(alternatives)
    ))
  }

  if (format %in% c("AudioMoth HEX", "AudioMoth")) {
    if (grepl("^[0-9]{8}_[0-9]{6}$", bn)) {
      # Newer AudioMoth filenames hold a wall clock rather than an instant, and
      # seewave::audiomoth() parses these in the session timezone whatever it is
      # passed, so they are parsed here instead.
      datetime <- .parseWallClock(bn, "%Y%m%d_%H%M%S", timezone)
    } else {
      data <- seewave::audiomoth(basename(file), tz=timezone)
      datetime <- data[,"time"]
      attr(datetime, "tzone") <- timezone
    }
    return(c(
      list(
        filename = file,
        match=format,
        datetime = datetime
      ),
      .ambiguityOf(alternatives)
    ))
  }
  if (format %in% c("Wildlife Acoustics SM2", "Wildlife Acoustics SM3")) {
    data <- seewave::songmeter(basename(file))
    # seewave::songmeter() parses the wall clock in the session timezone, so the
    # instant is rebuilt here from the parts of the filename it returns.
    ret <- (list(
      filename = file,
      match=format,
      datetime = ISOdatetime(
        data[,"year"], data[,"month"], data[,"day"],
        data[,"hour"], data[,"min"], data[,"sec"],
        tz=timezone
      ),
      model = data[,"model"],
      prefix = data[,"prefix"],
      mic = data[,"mic"],
      geo = data[,"geo"]
    ))
    return(c(ret, .ambiguityOf(alternatives)))
  }
  if (format == "YYYYMMDD") {
    datetime <- .parseWallClock(bn, "%Y%m%d", timezone)
  }
  if (format == "YYYYMMDD_HHMMSS") {
    datetime <- .parseWallClock(bn, "%Y%m%d_%H%M%S", timezone)
  }
  if (format == "timestamp") {
    #A timestamp counts from the epoch in UTC wherever it is read. Taking the
    #origin in whatever zone the session happened to be in moved the instant by
    #that zone's offset, and the timezone argument was not used at all.
    datetime <- as.POSIXct(
      as.numeric(bn),
      origin = as.POSIXct("1970-01-01", tz="UTC"),
      tz = timezone
    )
  }
  return(c(
    list(
      filename = file,
      match=format,
      datetime = datetime
    ),
    .ambiguityOf(alternatives)
  ))
}

#' Say that a reading was a guess, and what else it could have been
#'
#' Reported only where the name left the format in doubt, as the other fields
#' of a reading are: a Wildlife Acoustics name gives a model and a microphone
#' and nothing else does, so what a reading holds already depends on what the
#' name turned out to be, and a field that is absent whenever there is nothing
#' to say is the convention of this function rather than an exception to it.
#'
#' @param alternatives The formats the name could be in, or NULL where the
#'   format was not in doubt
#' @return A list to add to the reading, empty where it was not a guess
#' @keywords internal
#' @noRd
.ambiguityOf <- function(alternatives) {
  if (is.null(alternatives)) {
    return(list())
  }
  return(list(ambiguous=TRUE, alternatives=alternatives))
}

.parseWallClock <- function(bn, format, timezone) {
  return(as.POSIXct(strptime(bn, format, tz=timezone), tz=timezone))
}

.matchFormat <- function(file) {
  for (alternative in c(0, 1)) {
    formats <- lapply(file, .detectFormat, alternative=alternative)
    if (any(vapply(formats, is.null, logical(1)))) {
      next
    }
    same <- vapply(
      formats,
      function(f) identical(as.character(f), as.character(formats[[1]])),
      logical(1)
    )
    if (all(same)) {
      return(formats[[1]])
    }
  }
  stop("Could not find a single format matching all filenames")
}

.knownFileFormats <- function() {
  return(c(
    "AudioMoth HEX",
    "AudioMoth",
    "timestamp",
    "Wildlife Acoustics SM2",
    "Wildlife Acoustics SM3",
    "YYYYMMDD",
    "YYYYMMDD_HHMMSS",
    "TSA"
  ))
}

#' Report a format as one of several a filename could be in
#'
#' A name of nothing but digits is read here as a date or as a count of
#' seconds, but a collection that numbers its records gives names of exactly
#' that shape: bio.acousti.ca names a file by its record number, and
#' xeno-canto's numbers are approaching eight digits. Such a reading is a guess
#' among others, and says so, so that a caller putting it beside metadata read
#' from inside the file can decline to believe it.
#'
#' @param format The format that was picked
#' @param alternatives Every format the name could be in, that one included
#' @return The format, carrying the alternatives
#' @keywords internal
#' @noRd
.ambiguousFormat <- function(format, alternatives) {
  attr(format, "alternatives") <- alternatives
  return(format)
}

.detectFormat <- function(file, alternative=0) {
  bn <- tools::file_path_sans_ext(basename(file))
  # Check for timestamp
  if (grepl("^[0-9]{10}$", bn)) {
    return(.ambiguousFormat("timestamp", c("timestamp", "record number")))
  }
  # Check for YYYMMDD
  if (grepl("^[0-9]{8}$", bn)) {
    if (alternative == 1) {
      return("AudioMoth HEX")
    }
    return(.ambiguousFormat(
      "YYYYMMDD", c("YYYYMMDD", "AudioMoth HEX", "record number")
    ))
  }
  # Check for AudioMoth old hexadecimal
  if (grepl("^[0-9A-Fa-f]{8}$", bn)) {
    format <- "AudioMoth HEX"
    if (file_ext(file) != "wav") {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }

  # Check for YYYYMMDD_HHMMSS
  if (grepl("^[0-9]{8}_[0-9]{6}$", bn)) {
    return("YYYYMMDD_HHMMSS")
  }

  # Check for Wildlife Acoustics format PREFIX_YYYYMMDD_HHMMSS
  if (grepl("^[^_]*_[0-9]{8}_[0-9]{6}$", bn)) {
    format <- "Wildlife Acoustics SM2"
    if (!file_ext(file) %in% c("wav", "wac")) {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }

  # Check for Wildlife Acoustics SM3 format
  if (grepl("^[^_]*_[01+-]{3}_[0-9]{8}[_$][0-9]{6}$", bn)) {
    format <- "Wildlife Acoustics SM3"
    if (!file_ext(file) %in% c("wav", "wac")) {
      attr(format, "extension_match") <- FALSE
    }
    return(format)
  }

  # The hash is what makes a TSA name recognisable: the collections are many
  # and their accession numbers follow no one layout, so neither can be matched
  # on, where every name ends in the same sixteen hexadecimal characters.
  if (grepl(.TSA_PATTERN, bn)) {
    return("TSA")
  }
  return(NULL)
}

#' The shape of a TSA filename
#'
#' A collection, then the taxon or common name of the animal recorded with a
#' local accession number, then a hash of the file.
#' @keywords internal
#' @noRd
.TSA_PATTERN <- "^([A-Za-z][A-Za-z_]*)-(.+)-([0-9a-f]{16})$"

#' The shape of a scientific name inside a TSA filename
#'
#' A capitalised genus and a lowercase specific epithet, joined by an
#' underscore or, in some collections, by a hyphen. "spec." stands where the
#' species was not identified.
#' @keywords internal
#' @noRd
.TSA_BINOMIAL <- "^([A-Z][a-z]+)[_-]([a-z]+\\.?)(?:_(.*))?$"

#' Read what a TSA filename holds
#'
#' The collection and the hash are taken as written. A scientific name is read
#' where the name carries one, and what follows it is kept as the accession
#' number the collection gave the recording.
#'
#' Nothing is read as a taxon from a name that does not carry one in that
#' shape: part of the corpus is named by a German common name and a German word
#' for the kind of call, which this does not attempt to read, and reporting the
#' first words of such a name as a genus and a species would invent a taxon the
#' file was never given.
#'
#' @param bn A filename, without its directory or extension
#' @return A list of the parts, or NULL where the name is not of this shape
#' @keywords internal
#' @noRd
.tsaParts <- function(bn) {
  whole <- regmatches(bn, regexec(.TSA_PATTERN, bn))[[1]]
  if (length(whole) != 4L) {
    return(NULL)
  }
  parts <- list(
    collection = whole[2],
    taxon = NA_character_,
    accession = whole[3],
    hash = whole[4]
  )
  name <- regmatches(whole[3], regexec(.TSA_BINOMIAL, whole[3]))[[1]]
  if (length(name) >= 3L) {
    # A subspecies cannot be told from an accession number that happens to be
    # a bare word, so the first two parts are taken as the name and the rest as
    # the accession number, which is right for the great majority of the corpus.
    parts$taxon <- paste(name[2], name[3])
    parts$accession <- if (length(name) >= 4L && nzchar(name[4])) name[4] else NA_character_
  }
  return(parts)
}
