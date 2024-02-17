test_that("Reading config file", {
  f <- system.file("extdata/CONFIG.TXT", package="sonicscrewdriver")
  t <- audiomothConfig(f)
  expect_equal(typeof(t), "list")
  expect_equal(ncol(t), 2)
  expect_equal(names(t), c("Key", "Value"))

  # Check for standard keys
  expect_true(all(c("Device ID", "Firmware", "Gain") %in% t$Key))
})

test_that("Reading data from audiomoth wav file", {
  f <- system.file("extdata/AUDIOMOTH.WAV", package="sonicscrewdriver")
  d <- audiomothWave(f)
  expect_silent(audiomothWave(f))
  expect_equal(typeof(d), "list")
  expect_true(all(c("raw", "start_time", "start_date", "time_zone", "serial") %in% names(d)))

  f <- system.file("extdata/EMPTY.WAV", package="sonicscrewdriver")
  expect_equal(audiomothWave(f), list())
})
