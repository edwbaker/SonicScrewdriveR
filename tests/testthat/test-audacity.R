test_that("readAudacityLabels rejects incorrect input", {
  f <- system.file("extdata/Audacity_labels.txt", package="sonicscrewdriver")
  expect_error(readAudacityLabels(f, output="ewe"), "Unknown output format.")
})

test_that("readAudacityLabels returns Annotations correctly", {
  f <- system.file("extdata/Audacity_labels.txt", package="sonicscrewdriver")
  a <- readAudacityLabels(f)

  expect_type(a, "list")
  expect_s4_class(a[[1]], "Annotation")
  expect_s4_class(a[[2]], "Annotation")
  expect_equal(basename(a[[1]]@file), "Audacity_labels.txt")
  expect_equal(a[[2]]@start, 2)
  expect_equal(a[[2]]@end, 3)
  expect_equal(a[[1]]@value, "Test Label 1")
  expect_equal(a[[2]]@source, "readAudacityLabels")
})

test_that("readAudacityLabels returns data.frame correctly", {
  f <- system.file("extdata/Audacity_labels.txt", package="sonicscrewdriver")
  a <- readAudacityLabels(f, output="data.frame")

  expect_type(a, "list")
  expect_equal(nrow(a), 2)
  # Frequency limits are read from the file's continuation rows, and default to
  # the full range for a label file that does not give them.
  expect_equal(ncol(a), 5)
  expect_equal(colnames(a), c("from", "to", "label", "low", "high"))
  expect_equal(a$low, c(0, 0))
  expect_equal(a$high, c(Inf, Inf))
  expect_equal(a$from[2], 2)
  expect_equal(a$to[2], 3)
  expect_equal(a$label[1], "Test Label 1")
})

test_that("read-write-read test", {
  f <- system.file("extdata/Audacity_labels.txt", package="sonicscrewdriver")
  b <- readAudacityLabels(f)
  writeAudacityLabels(b, "test_labels.txt")
  a <- readAudacityLabels("test_labels.txt")

  expect_type(a, "list")
  expect_s4_class(a[[1]], "Annotation")
  expect_s4_class(a[[2]], "Annotation")
  expect_equal(basename(a[[1]]@file), "test_labels.txt")
  expect_equal(a[[2]]@start, 2)
  expect_equal(a[[2]]@end, 3)
  expect_equal(a[[1]]@value, "Test Label 1")
  expect_equal(a[[2]]@source, "readAudacityLabels")

  unlink("test_labels.txt")
})

test_that("Audacity labels with frequencies survive a round trip", {
  # The five column form puts the frequency limits on a continuation row starting
  # with a backslash. Writing named the columns so that seewave could not find
  # them, giving a file with neither times nor frequencies, and reading treated
  # the continuation row as a label of its own.
  f <- tempfile(fileext=".txt")
  a <- list(
    annotation(start=1, end=2, low=1000, high=2000, value="one"),
    annotation(start=3, end=4, low=500, high=1500, value="two")
  )
  writeAudacityLabels(a, f)

  written <- readLines(f)
  expect_equal(length(written), 4)
  expect_match(written[1], "^1\t2\tone$")
  expect_match(written[2], "^\\\\\t1000\t2000$")

  b <- readAudacityLabels(f)
  expect_equal(length(b), 2)
  expect_equal(sapply(b, function(x) x@start), c(1, 3))
  expect_equal(sapply(b, function(x) x@end), c(2, 4))
  expect_equal(sapply(b, function(x) x@low), c(1000, 500))
  expect_equal(sapply(b, function(x) x@high), c(2000, 1500))
  expect_equal(sapply(b, function(x) x@value), c("one", "two"))
})

test_that("Audacity labels without frequencies use the three column form", {
  f <- tempfile(fileext=".txt")
  a <- list(annotation(start=1, end=2, value="one"))
  writeAudacityLabels(a, f)
  expect_equal(length(readLines(f)), 1)
  expect_equal(readAudacityLabels(f, output="data.frame")$from, 1)
})
