#' The layouts BirdNET Analyser writes its results in
#'
#' BirdNET writes a results file in one of several layouts, and which columns
#' it holds and which character separates them both depend on the layout and on
#' the release. The names are those of `save_as_rtable()`, `save_as_csv()` and
#' `save_as_kaleidoscope()` in BirdNET-Analyzer, and of the `RAVEN_TABLE_HEADER`,
#' `CSV_HEADER`, `RTABLE_HEADER` and `KALEIDOSCOPE_HEADER` of its 1.x releases,
#' rather than names guessed from files in hand.
#'
#' `start` and `end` are the columns holding the begin and end of a detection in
#' seconds from the start of the recording, under the names `read.csv()` leaves
#' them in. Kaleidoscope writes a duration instead of an end, so an end read
#' from it without adding the offset would be wrong for every detection after
#' the first.
#'
#' Matching is on the time columns alone. They are unique to each layout, and
#' the rest of the columns are not a fixed set: the `csv` layout takes eight
#' more on request, and the selection table was reordered and gained a
#' `Scientific Name` between the 1.x releases and the current one.
#'
#' The Audacity labels BirdNET also writes carry no header at all, so they
#' cannot be read as a table of named columns. [readAudacityLabels()] reads
#' those.
#' @keywords internal
#' @noRd
.BIRDNET_LAYOUTS <- list(
  list(
    name = "Raven selection table",
    sep = "\t",
    start = "Begin.Time..s.",
    end = "End.Time..s."
  ),
  list(
    name = "csv",
    sep = ",",
    start = "Start..s.",
    end = "End..s."
  ),
  list(
    name = "rtable",
    sep = ",",
    start = "start",
    end = "end"
  ),
  list(
    name = "Kaleidoscope",
    sep = ",",
    start = "OFFSET",
    duration = "DURATION"
  )
)

#' Which layout a BirdNET results file is in
#'
#' The separator is part of what distinguishes them, so it is found rather than
#' assumed: the selection table is tab separated and the other three are comma
#' separated, and reading a comma separated file as tab separated gives one
#' column whose name is the whole header.
#'
#' @param file Path to a results file
#' @return One entry of `.BIRDNET_LAYOUTS`, carrying the columns it was read
#'   with, or NULL where the file is in none of them
#' @keywords internal
#' @noRd
.birdNetLayout <- function(file) {
  for (sep in unique(vapply(.BIRDNET_LAYOUTS, function(l) l$sep, character(1)))) {
    # The header alone, read by the same parser that will read the body, so
    # that the names are mangled and any quoting handled exactly as they will
    # be. A file with a header and no detections is a layout all the same.
    columns <- tryCatch(
      colnames(read.csv(file, sep=sep, nrows=1)),
      error = function(cond) character(0)
    )
    for (layout in .BIRDNET_LAYOUTS) {
      wanted <- c(layout$start, layout$end, layout$duration)
      if (identical(layout$sep, sep) && all(wanted %in% columns)) {
        layout$columns <- columns
        return(layout)
      }
    }
  }
  return(NULL)
}

#' Say that a file is in no layout this knows
#'
#' Named so that the message says what is wrong with the file, where assigning
#' to an absent column reported only "replacement has 0 rows, data has 1".
#'
#' @param file The file that could not be read
#' @return A one-line description of the file and what was expected
#' @keywords internal
#' @noRd
.birdNetLayoutMessage <- function(file) {
  first <- tryCatch(readLines(file, n=1, warn=FALSE), error=function(cond) "")
  if (length(first) == 0L) {
    first <- ""
  }
  known <- paste(
    vapply(.BIRDNET_LAYOUTS, function(l) l$name, character(1)),
    collapse = ", "
  )
  out <- paste0(
    basename(file), " is in none of the BirdNET results layouts this reads (",
    known, "). Its first line is: ", first
  )
  # The Audacity labels BirdNET writes have no header, so the first line is a
  # detection. This package reads those, just not here.
  fields <- strsplit(first, "\t", fixed=TRUE)[[1]]
  if (length(fields) >= 3L && !is.na(suppressWarnings(as.numeric(fields[1])))) {
    out <- paste0(
      out, ". It looks like a file of Audacity labels, which ",
      "readAudacityLabels() reads."
    )
  }
  return(out)
}

#' The clock time a BirdNET results file's own name gives
#'
#' @param filename The name of the results file, without its directory
#' @param filename_parsing Which convention to read it by
#' @return The time the recording started, or NULL where no name is to be read
#' @keywords internal
#' @noRd
.birdNetStart <- function(filename, filename_parsing) {
  if (identical(filename_parsing, "audiomoth")) {
    # A name that is not of that shape gave "subscript out of bounds", or a
    # whole table of NA times where it had two parts that were not a date.
    if (!grepl("^[0-9]{8}_[0-9]{6}", filename)) {
      stop(
        filename, " is not an AudioMoth filename, which filename_parsing=",
        '"audiomoth" reads as YYYYMMDD_HHMMSS. No times are added to a ',
        "reading rather than every detection being given a time of NA."
      )
    }
    parts <- strsplit(filename, "_")[[1]]
    return(as.POSIXct(
      paste(parts[[1]], parts[[2]]), format="%Y%m%d %H%M%OS"
    ))
  }
  if (identical(filename_parsing, "timestamp")) {
    seconds <- strsplit(filename, ".", fixed=TRUE)[[1]][1]
    if (!grepl("^[0-9]+$", seconds)) {
      stop(
        filename, " is not a Unix timestamp, which filename_parsing=",
        '"timestamp" reads as a count of seconds before the first dot of the ',
        "name. No times are added to a reading rather than every detection ",
        "being given a time of NA."
      )
    }
    return(as.POSIXct(as.numeric(seconds), origin="1970-01-01"))
  }
  return(NULL)
}

#' Read one BirdNET results file
#' @keywords internal
#' @noRd
.birdNetFile <- function(file, filename_parsing, layout) {
  ret <- read.csv(file, sep=layout$sep)
  fn <- rep_len(file, nrow(ret))
  cn <- colnames(ret)
  ret <- cbind(fn, ret)
  colnames(ret) <- c("filename", cn)

  #The times are coerced wherever the layout keeps them rather than at one
  #hard-coded name. Assigning to `Begin.Time..s.` whatever the file held gave
  #"replacement has 0 rows, data has 1" for every layout but the selection
  #table, which says nothing about BirdNET columns.
  ret[[layout$start]] <- as.numeric(ret[[layout$start]])
  if (!is.null(layout$end)) {
    ret[[layout$end]] <- as.numeric(ret[[layout$end]])
  }
  if (!is.null(layout$duration)) {
    ret[[layout$duration]] <- as.numeric(ret[[layout$duration]])
  }

  #basename() takes the last element of the path whichever separator it uses.
  #Splitting on a forward slash left the whole path in hand on Windows, where
  #a path holds backslashes, and the filename parsing below then read the
  #directories rather than the name.
  start <- .birdNetStart(basename(file), filename_parsing)
  if (is.null(start)) {
    return(ret)
  }
  from <- ret[[layout$start]]
  to <- if (is.null(layout$duration)) {
    ret[[layout$end]]
  } else {
    # Kaleidoscope writes how long a detection lasts, not when it ends.
    from + ret[[layout$duration]]
  }
  cn <- colnames(ret)
  ret <- cbind(ret, start + from, start + to)
  colnames(ret) <- c(cn, "Start", "End")
  return(ret)
}

#' Read a directory of BirdNET results files
#'
#' A file in no layout this knows is passed over with a warning naming it,
#' rather than refusing the directory: pointing this at one file that is not a
#' results file is a mistake worth refusing, where one stray file among nine
#' hundred is not a reason to abandon the other eight hundred and ninety-nine.
#' A results directory commonly holds such files, as BirdNET writes its
#' analysis parameters beside its results.
#'
#' @param file The directory
#' @param filename_parsing Which convention to read each name by
#' @return The detections of every results file in it
#' @keywords internal
#' @noRd
.birdNetDirectory <- function(file, filename_parsing) {
  paths <- list.files(file, full.names=TRUE)
  # Subdirectories were read as files, which gave the error of whichever file
  # list.files() returned first from inside them.
  paths <- paths[!dir.exists(paths)]
  layouts <- lapply(paths, .birdNetLayout)
  unreadable <- vapply(layouts, is.null, logical(1))
  for (path in paths[unreadable]) {
    .unparsedWarning(
      "unparsedResults",
      "Passing over ", .birdNetLayoutMessage(path)
    )
  }
  paths <- paths[!unreadable]
  layouts <- layouts[!unreadable]

  if (length(paths) == 0L) {
    .unparsedWarning(
      "unparsedResults",
      file, " holds no BirdNET results file this reads, so there are no ",
      "detections to report."
    )
    # An empty data frame rather than the NULL that do.call(rbind, list())
    # gives, so that a caller going straight on to index a column gets an
    # empty column rather than an error about NULL.
    return(data.frame(filename=character(0)))
  }

  names <- vapply(layouts, function(l) l$name, character(1))
  if (length(unique(names)) > 1L) {
    # The columns of two layouts do not line up, so rbind() would fail with
    # "numbers of columns of arguments do not match". Refusing by name says
    # which layouts, and it is the right answer rather than a limitation: a
    # BirdNET run writes a combined table beside its per-file tables, and that
    # table holds the same detections again, so a reading that stacked the two
    # would count every detection twice.
    examples <- vapply(
      unique(names),
      function(n) basename(paths[names == n][1]),
      character(1)
    )
    stop(
      file, " holds BirdNET results in more than one layout, which hold ",
      "different columns and cannot be read as one table: ",
      paste0(unique(names), " (", examples, ")", collapse=", "),
      ". Read them a layout at a time, or separate them. Note that a ",
      "combined table holds the same detections as the per-file tables ",
      "beside it, so reading both would count each detection twice."
    )
  }

  # Unnamed, as the lapply() this replaces was: rbind() takes the names of a
  # named list of data frames as row names, which would make them paths.
  return(do.call(rbind, unname(Map(
    function(path, layout) .birdNetFile(path, filename_parsing, layout),
    paths, layouts
  ))))
}

#' Read output files from BirdNet Analyser
#'
#' Reads a single file, or directory of files, output by BirdNet Analyser.
#'
#' @details
#' BirdNET writes its results in one of several layouts, which hold different
#' columns and are not all separated by the same character, and which have
#' changed across its releases. The layout is worked out from the file's own
#' header rather than assumed, so a Raven selection table, a `csv`, an
#' `rtable` and a Kaleidoscope file all read, and the begin and end of each
#' detection are taken from whichever columns that layout keeps them in. A
#' Kaleidoscope file gives how long a detection lasts rather than when it ends,
#' and an end is worked out from it.
#'
#' A file in none of those layouts is refused by name, with its first line and
#' the layouts that are known, where assigning to a column the file does not
#' have reported only `replacement has 0 rows, data has 1`. In a directory such
#' a file is passed over with a warning instead, since a results directory
#' commonly holds files that are not results.
#'
#' A `filename_parsing` the name cannot be read by is an error naming the file
#' and the convention asked for. Reading an AudioMoth name as a timestamp gave
#' every detection a time of NA and said only `NAs introduced by coercion`.
#'
#' @param file Filename or directory
#' @param filename_parsing Allows for filename parsing, accepted values are one of none, audiomoth, timestamp.
#' @export
#' @return A data frame. A directory holding no results file this reads gives
#'   one of no rows, with a warning, rather than NULL.
#' @seealso [readAudacityLabels()] reads the Audacity labels BirdNET also
#'   writes, which carry no header.
readBirdNet <- function(file, filename_parsing="none") {
  .validateChoice(
    filename_parsing, c("none", "audiomoth", "timestamp"),
    msg=paste("Unknown filename_parsing for readBirdNet:", filename_parsing)
  )
  if (file.exists(file) && !dir.exists(file)) {
    layout <- .birdNetLayout(file)
    if (is.null(layout)) {
      stop(.birdNetLayoutMessage(file))
    }
    return(.birdNetFile(file, filename_parsing, layout))
  }
  if (dir.exists(file)) {
    return(.birdNetDirectory(file, filename_parsing))
  }
  stop(paste("No such file or directory:", file))
}
