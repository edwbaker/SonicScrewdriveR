test_that("birdNetAnalyse() rejects incorrect input", {
  skip_if_no_py_module("birdnetlib")
  expect_error(birdNetAnalyse("filename", output="christmas cactus"), "Unknown output format.")
  expect_error(birdNetAnalyse("filename", lat=1), "If lat is provided, lon must also be provided.")
})

test_that("birdNetAnalyse() returns correct format", {
  skip_if_no_py_module("birdnetlib")
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  f <- c(f,f)
  df <- birdNetAnalyse(f, output="data.frame")
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 11)

  a <- birdNetAnalyse(f, output="Annotation")
  expect_type(a, "list")
  expect_true(all(sapply(a, is, "Annotation")))

  a <- birdNetAnalyse(f, lat=54, lon=0, output="Annotation")
  expect_type(a, "list")
  expect_true(all(sapply(a, is, "Annotation")))

  a <- birdNetAnalyse(f, lat=54, lon=0, date=as.Date("2024-06-30"), output="Annotation")
  expect_type(a, "list")
  expect_true(all(sapply(a, is, "Annotation")))
})
