#A BirdNET Analyzer selection table, which is tab separated and holds the
#detections of one recording. Only the begin and end times are read by
#readBirdNet(), but the rest of the columns are kept so that the shape of the
#result can be checked.
writeBirdNetTable <- function(path, begin=c(0, 3), end=c(3, 6)) {
  rows <- data.frame(
    "Selection" = seq_along(begin),
    "View" = "Spectrogram 1",
    "Channel" = 1,
    "Begin Time (s)" = begin,
    "End Time (s)" = end,
    "Low Freq (Hz)" = 150,
    "High Freq (Hz)" = 12000,
    "Species Code" = c("eurbla", "comcha"),
    "Common Name" = c("Eurasian Blackbird", "Common Chaffinch"),
    "Confidence" = c(0.8, 0.5),
    check.names = FALSE
  )
  write.table(rows, path, sep="\t", row.names=FALSE, quote=FALSE)
  return(path)
}

birdNetFile <- function(name, ...) {
  dir <- file.path(tempdir(), paste0("birdnet-", basename(tempfile(""))))
  dir.create(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE), envir=parent.frame())
  writeBirdNetTable(file.path(dir, name), ...)
}

test_that("a missing file is reported by name rather than read", {
  missing <- file.path(tempdir(), "no-such-birdnet-file.txt")
  expect_error(readBirdNet(missing), "No such file or directory")
  expect_error(readBirdNet(missing), basename(missing), fixed=TRUE)
})

test_that("the filename the detections came from is the first column", {
  path <- birdNetFile("20240615_103000.txt")
  res <- readBirdNet(path)
  expect_equal(colnames(res)[1], "filename")
  expect_equal(unique(res$filename), path)
  expect_equal(nrow(res), 2)
})

test_that("the columns of the selection table are kept after the filename", {
  path <- birdNetFile("20240615_103000.txt")
  res <- readBirdNet(path)
  expect_equal(
    colnames(res),
    c("filename", "Selection", "View", "Channel", "Begin.Time..s.",
      "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.", "Species.Code",
      "Common.Name", "Confidence")
  )
  expect_equal(res$Common.Name, c("Eurasian Blackbird", "Common Chaffinch"))
})

test_that("the begin and end times are numeric", {
  path <- birdNetFile("20240615_103000.txt", begin=c(0, 3), end=c(3, 6))
  res <- readBirdNet(path)
  expect_type(res$Begin.Time..s., "double")
  expect_type(res$End.Time..s., "double")
  expect_equal(res$Begin.Time..s., c(0, 3))
  expect_equal(res$End.Time..s., c(3, 6))
})

test_that("no parsing is asked for by default, so no times are added", {
  path <- birdNetFile("20240615_103000.txt")
  res <- readBirdNet(path)
  expect_false("Start" %in% colnames(res))
  expect_false("End" %in% colnames(res))
})

test_that("an AudioMoth filename gives the clock time of each detection", {
  path <- birdNetFile("20240615_103000.txt", begin=c(0, 3), end=c(3, 6))
  res <- readBirdNet(path, filename_parsing="audiomoth")
  start <- as.POSIXct("20240615 103000", format="%Y%m%d %H%M%OS")
  expect_equal(res$Start, start + c(0, 3))
  expect_equal(res$End, start + c(3, 6))
})

test_that("the parsed filename is the name alone, not the directory it is in", {
  #The path of a temporary file holds separators and, on Windows, backslashes.
  #Reading the whole path as the name gave NA times.
  path <- birdNetFile("20240615_103000.txt")
  res <- readBirdNet(path, filename_parsing="audiomoth")
  expect_false(any(is.na(res$Start)))
  expect_false(any(is.na(res$End)))
})

test_that("a timestamp filename gives the clock time of each detection", {
  path <- birdNetFile("1718445000.txt", begin=c(0, 3), end=c(3, 6))
  res <- readBirdNet(path, filename_parsing="timestamp")
  start <- as.POSIXct(1718445000, origin="1970-01-01")
  expect_equal(as.numeric(res$Start), as.numeric(start + c(0, 3)))
  expect_equal(as.numeric(res$End), as.numeric(start + c(3, 6)))
})

test_that('filename_parsing="none" gives the file\'s columns and no times', {
  path <- birdNetFile("20240615_103000.txt")
  res <- readBirdNet(path, filename_parsing="none")
  expect_equal(ncol(res), 11)
})

test_that("a directory is read as every selection table in it", {
  dir <- file.path(tempdir(), paste0("birdnet-dir-", basename(tempfile(""))))
  dir.create(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  writeBirdNetTable(file.path(dir, "20240615_103000.txt"), begin=c(0, 3), end=c(3, 6))
  writeBirdNetTable(file.path(dir, "20240615_113000.txt"), begin=c(1, 4), end=c(4, 7))

  res <- readBirdNet(dir)
  expect_equal(nrow(res), 4)
  expect_length(unique(res$filename), 2)
  expect_equal(sort(res$Begin.Time..s.), c(0, 1, 3, 4))
})

test_that("the filename of each row in a directory is its own file", {
  dir <- file.path(tempdir(), paste0("birdnet-dir-", basename(tempfile(""))))
  dir.create(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE))
  writeBirdNetTable(file.path(dir, "20240615_103000.txt"))
  writeBirdNetTable(file.path(dir, "20240615_113000.txt"))

  res <- readBirdNet(dir, filename_parsing="audiomoth")
  #The two recordings are an hour apart, so parsing each row against its own
  #file gives two distinct hours rather than one repeated.
  expect_length(unique(format(res$Start, "%H")), 2)
})

# BirdNET writes its results in one of several layouts, which hold different
# columns and are not all separated by the same character. readBirdNet() read
# every file as tab separated and assigned to `Begin.Time..s.` whatever the
# file held, so three of the four layouts failed with "replacement has 0 rows,
# data has 1", which says nothing about BirdNET columns. The headers below are
# verbatim from the source of BirdNET-Analyzer: RAVEN_TABLE_HEADER, CSV_HEADER,
# RTABLE_HEADER and KALEIDOSCOPE_HEADER of its 1.x releases, and the columns of
# save_as_rtable(), save_as_csv() and save_as_kaleidoscope() on its main.

birdNetLayoutFile <- function(dir, name, layout) {
  lines <- switch(
    layout,
    raven = c(
      paste("Selection", "View", "Channel", "Begin Time (s)", "End Time (s)",
            "Low Freq (Hz)", "High Freq (Hz)", "Common Name", "Species Code",
            "Confidence", "Begin Path", "File Offset (s)", sep="\t"),
      paste(1, "Spectrogram 1", 1, "0.0", "3.0", 150, 12000,
            "Eurasian Blackbird", "eurbla", 0.8, "/x.wav", "0.0", sep="\t"),
      paste(2, "Spectrogram 1", 1, "3.0", "6.0", 150, 12000,
            "Common Chaffinch", "comcha", 0.5, "/x.wav", "3.0", sep="\t")
    ),
    csv = c(
      "Start (s),End (s),Scientific name,Common name,Confidence,File",
      "0.0,3.0,Turdus merula,Eurasian Blackbird,0.8,/x.wav",
      "3.0,6.0,Fringilla coelebs,Common Chaffinch,0.5,/x.wav"
    ),
    rtable = c(
      paste0("filepath,start,end,scientific_name,common_name,confidence,lat,",
             "lon,week,overlap,sensitivity,min_conf,species_list,model"),
      "/x.wav,0.0,3.0,Turdus merula,Eurasian Blackbird,0.8,-1,-1,-1,0,1,0.1,,BirdNET",
      "/x.wav,3.0,6.0,Fringilla coelebs,Common Chaffinch,0.5,-1,-1,-1,0,1,0.1,,BirdNET"
    ),
    # OFFSET and DURATION, where DURATION is how long a detection lasts and
    # not when it ends.
    kaleidoscope = c(
      paste0("INDIR,FOLDER,IN FILE,OFFSET,DURATION,scientific_name,",
             "common_name,confidence,lat,lon,week,overlap,sensitivity"),
      "/in,f,x.wav,0.0,3.0,Turdus merula,Eurasian Blackbird,0.8,-1,-1,-1,0,1",
      "/in,f,x.wav,3.0,3.0,Fringilla coelebs,Common Chaffinch,0.5,-1,-1,-1,0,1"
    )
  )
  path <- file.path(dir, name)
  writeLines(lines, path)
  return(path)
}

birdNetLayoutDir <- function() {
  dir <- file.path(tempdir(), paste0("birdnet-l-", basename(tempfile(""))))
  dir.create(dir, recursive=TRUE)
  withr::defer(unlink(dir, recursive=TRUE), envir=parent.frame())
  return(dir)
}

birdNetLayouts <- c("raven", "csv", "rtable", "kaleidoscope")

test_that("Every layout BirdNET writes is read", {
  dir <- birdNetLayoutDir()
  for (layout in birdNetLayouts) {
    path <- birdNetLayoutFile(dir, paste0("20240615_103000.", layout), layout)
    res <- readBirdNet(path)
    expect_equal(nrow(res), 2, info=layout)
    expect_equal(colnames(res)[1], "filename", info=layout)
    # The columns of the file itself are kept, so nothing is renamed to a
    # canonical set the file never had.
    expect_true(ncol(res) > 2, info=layout)
  }
})

test_that("The begin and end of a detection are numeric in every layout", {
  dir <- birdNetLayoutDir()
  for (layout in birdNetLayouts) {
    path <- birdNetLayoutFile(dir, paste0("20240615_103000.", layout), layout)
    res <- readBirdNet(path)
    begin <- res[[.birdNetLayout(path)$start]]
    expect_type(begin, "double")
    expect_equal(begin, c(0, 3), info=layout)
  }
})

test_that("Every layout gives the same clock time for the same detections", {
  # The point of detecting the layout: these four files hold one recording's
  # detections written four ways, so the times have to agree.
  dir <- birdNetLayoutDir()
  for (layout in birdNetLayouts) {
    path <- birdNetLayoutFile(dir, paste0("20240615_103000.", layout), layout)
    res <- readBirdNet(path, filename_parsing="audiomoth")
    start <- as.POSIXct("20240615 103000", format="%Y%m%d %H%M%OS")
    expect_equal(res$Start, start + c(0, 3), info=layout)
    # The second Kaleidoscope detection lasts 3s from an offset of 3s, so it
    # ends at 6s. A duration read as an end would give 3s.
    expect_equal(res$End, start + c(3, 6), info=layout)
  }
})

test_that("A file in no layout this knows is refused by name", {
  dir <- birdNetLayoutDir()
  path <- file.path(dir, "notes.csv")
  writeLines(c("a,b", "1,2"), path)

  expect_error(readBirdNet(path), "notes.csv", fixed=TRUE)
  expect_error(readBirdNet(path), "none of the BirdNET results layouts")
  # The layouts that are known, and the line that was not one of them, so the
  # message says enough to act on.
  expect_error(readBirdNet(path), "Raven selection table", fixed=TRUE)
  expect_error(readBirdNet(path), "a,b", fixed=TRUE)
})

test_that("Audacity labels are named as such rather than read here", {
  # BirdNET writes these too, and they carry no header at all, so the first
  # detection in one would be read as the header.
  dir <- birdNetLayoutDir()
  path <- file.path(dir, "BirdNET_AudacityLabels.txt")
  writeLines("0.0\t3.0\tTurdus merula_Eurasian Blackbird\t0.8", path)

  expect_error(readBirdNet(path), "readAudacityLabels", fixed=TRUE)
})

test_that("A timestamp parsing the name cannot be read by is an error", {
  # This gave every detection a time of NA, warning only "NAs introduced by
  # coercion", which names neither the file nor the parsing asked for.
  path <- birdNetFile("20190101_123000.BirdNET.results.txt")
  expect_error(readBirdNet(path, "timestamp"), "not a Unix timestamp")
  expect_error(readBirdNet(path, "timestamp"),
               "20190101_123000.BirdNET.results.txt", fixed=TRUE)
  # The same file read by the convention its name does follow.
  expect_false(any(is.na(readBirdNet(path, "audiomoth")$Start)))
})

test_that("An audiomoth parsing the name cannot be read by is an error", {
  path <- birdNetFile("1718445000.txt")
  expect_error(readBirdNet(path, "audiomoth"), "not an AudioMoth filename")
  expect_false(any(is.na(readBirdNet(path, "timestamp")$Start)))
})

test_that("An unknown filename_parsing is refused", {
  path <- birdNetFile("20240615_103000.txt")
  expect_error(readBirdNet(path, "bogus"), "Unknown filename_parsing")
})

test_that("A directory passes over a file that is not results", {
  # A results directory holds the analysis parameters BirdNET writes beside
  # them, and may hold subdirectories of segments. One stray file is no reason
  # to abandon the rest of the walk.
  dir <- birdNetLayoutDir()
  writeBirdNetTable(file.path(dir, "20240615_103000.txt"), begin=c(0, 3))
  writeBirdNetTable(file.path(dir, "20240615_113000.txt"), begin=c(1, 4))
  writeLines(c("setting,value", "min_conf,0.1"),
             file.path(dir, "birdnet.analyze-params.csv"))
  dir.create(file.path(dir, "segments"))

  expect_warning(readBirdNet(dir), "birdnet.analyze-params.csv", fixed=TRUE)
  res <- suppressWarnings(readBirdNet(dir))
  expect_equal(nrow(res), 4)
  expect_length(unique(res$filename), 2)
  # The row names are the count, as they were when every file was read blindly.
  expect_equal(rownames(res), as.character(1:4))

  w <- tryCatch(readBirdNet(dir), warning=function(w) w)
  expect_s3_class(w, "unparsedResults")
  expect_s3_class(w, "unparsedField")
})

test_that("A directory of more than one layout is refused by name", {
  # A BirdNET run writes a combined table beside its per-file tables, holding
  # the same detections again. Their columns do not line up, so rbind() failed
  # with "numbers of columns of arguments do not match"; and stacking them
  # would count every detection twice.
  dir <- birdNetLayoutDir()
  birdNetLayoutFile(dir, "20240615_103000.txt", "raven")
  birdNetLayoutFile(dir, "BirdNET_CombinedTable.csv", "csv")

  expect_error(readBirdNet(dir), "more than one layout")
  expect_error(readBirdNet(dir), "Raven selection table", fixed=TRUE)
  expect_error(readBirdNet(dir), "BirdNET_CombinedTable.csv", fixed=TRUE)
  expect_error(readBirdNet(dir), "count each detection twice")
})

test_that("A directory holding no results gives no rows rather than NULL", {
  dir <- birdNetLayoutDir()
  expect_warning(readBirdNet(dir), "no BirdNET results file")
  res <- suppressWarnings(readBirdNet(dir))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  # A data frame rather than the NULL that do.call(rbind, list()) gives, so
  # that code going straight on to a column gets an empty one, not an error.
  expect_identical(res$filename, character(0))
})
