test_that("dielLabels rejects unknown format", {
  expect_error(dielLabels("neon tetra"), "Unknown format for dielLabels: neon tetra")
})

test_that("dielLabels gives expected output", {
  l <- dielLabels("clock24")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)

  l <- dielLabels("clock12")
  expect_equal(typeof(l), "character")
  expect_equal(length(l), 8)
})

test_that("dielPositions rejects unknown format", {
  expect_error(dielPositions("aardvark"), "Unknown format for dielPositions: aardvark")
})

test_that("dielPositions gives expected output", {
  l <- dielPositions("3hourly")
  expect_true(is.numeric(l))
  expect_equal(length(l), 8)

  l <- dielPositions("hours")
  expect_true(is.numeric(l))
  expect_equal(length(l), 24)

  l <- dielPositions("minutes")
  expect_true(is.numeric(l))
  expect_equal(length(l), 1440)
})
