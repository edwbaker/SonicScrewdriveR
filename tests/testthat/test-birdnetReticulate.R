skip_if_no_birdnet <- function() {
  if (!reticulate::virtualenv_exists(envname = "ssd_birdnet")) {
    skip("ssd_birdnet not available for testing")
  }
  if (!reticulate::py_module_available("birdnetlib"))
    skip("birdnetlib not available for testing")
}

test_that("birdNetAnalyse() rejects incorrect input", {
  skip_if_no_birdnet()
  expect_error(birdNetAnalyse("filename", output="christmas cactus"), "Unknown output format.")
  expect_error(birdNetAnalyse("filename", lat=1), "If lat is provided, lon must also be provided.")
})

test_that("birdNetAnalyse() returns correct format", {
  skip_if_no_birdnet()
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  f <- c(f,f)
  df <- birdNetAnalyse(f, output="data.frame")
  expect_type(df, "list")
  expect_equal(ncol(df), 7)

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
