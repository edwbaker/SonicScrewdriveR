test_that("pulse rejects unknown types", {
  expect_error(pulse("nightshade"), "pulse type not recognised.")
  expect_silent(pulse("dirac"))
})

test_that("pulse is correct length", {
  expect_error(pulse(duration=24, samp.rate=230), "leading cannot be greater than duration.")
  expect_equal(length(pulse(duration=23, leading=0)), 23)
  expect_equal(pulse(duration=23, leading=1, samp.rate=5)@samp.rate, 5)
})

test_that("pulse is correct type", {
  p <- pulse(bit=8, pcm=TRUE)
  expect_equal(p@bit, 8)
  expect_equal(p@pcm, TRUE)

  p <- pulse(bit=32, pcm=FALSE)
  expect_equal(p@bit, 32)
  expect_equal(p@pcm, FALSE)
})

test_that("dirac pulse works", {
  expect_equal(2 * 2, 4)
})

test_that("dirac pulse works as expected", {
  p <- pulse("dirac", leading=0)
  expect_equal(p@left[1], 2^p@bit - 1)
  expect_true(all(p@left[2:length(p@left)] == 0))

  p <- pulse("dirac", leading=1)
  expect_equal(p@left[1], 0)
  expect_equal(p@left[2], 2^p@bit - 1)
  expect_true(all(p@left[3:length(p@left)] == 0))
})
