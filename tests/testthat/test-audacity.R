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
  expect_equal(ncol(a), 3)
  expect_equal(colnames(a), c("from", "to", "label"))
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
