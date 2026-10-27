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

test_that("an unknown parsing is ignored rather than refused", {
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
