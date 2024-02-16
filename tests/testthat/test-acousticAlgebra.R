test_that(".concat rejects invalid methods", {
  w1 <- sine(440, samp.rate=44100)
  w2 <- sine(440, samp.rate=44100)

  expect_error(concat(w1, w2, method="horsehoe crab"), "Unknown concatenation method.")
})

test_that("bind method on Wave objects works", {
  w1 <- sine(440, samp.rate=44100)
  w2 <- sine(440, samp.rate=44100)

  w3 <- concat(w1, w2, method="bind")
  expect_true(is(w3, "Wave"))
  expect_equal(length(w3@left), length(w1@left) + length(w2@left))
  expect_equal(length(w3@right), length(w1@right) + length(w2@right))
})

test_that("noClick method on Wave objects works", {
  w1 <- sine(10.3, samp.rate=450)

  w2 <- concat(w1, w1, method="noClick")
  expect_true(is(w2, "Wave"))
  expect_lt(length(w2@left), length(w1@left)*2)

  w3 <- concat(w1, w1, w1, method="noClick")
  expect_true(is(w3, "Wave"))
  expect_lt(length(w3@left), length(w1@left)*3)
})

test_that("bind method on WaveMC objects works", {
  w1 <- sine(440, samp.rate=44100)
  w2 <- sine(440, samp.rate=44100)
  w1 <- tuneR::WaveMC(w1)
  w2 <- tuneR::WaveMC(w2)

  w3 <- concat(w1, w2, method="bind")
  expect_true(is(w3, "WaveMC"))
  expect_equal(nrow(w3@.Data), nrow(w1@.Data) + nrow(w2@.Data))
})

test_that("noClick method on WaveMC objects works", {
  w1 <- tuneR::WaveMC(sine(10.3, samp.rate=450))

  w2 <- concat(w1, w1, method="noClick")
  expect_true(is(w2, "WaveMC"))
  expect_lt(nrow(w2@.Data), nrow(w1@.Data)*2)

  w3 <- concat(w1, w1, w1, method="noClick")
  expect_true(is(w3, "WaveMC"))
  expect_lt(nrow(w3@.Data), nrow(w1@.Data)*3)
})

test_that("bind method on TaggedWave objects works", {
  w1 <- sine(440, samp.rate=44100)
  w2 <- sine(440, samp.rate=44100)
  w1 <- tagWave(w1)
  w2 <- tagWave(w2)
  w2@metadata <- list("test" = "value")

  w3 <- concat(w1, w2, method="bind")
  expect_true(is(w3, "TaggedWave"))
  expect_equal(length(w3@left), length(w1@left) + length(w2@left))
  expect_equal(length(w3@right), length(w1@right) + length(w2@right))
  expect_equal(w3@processing[[1]]$process, "concat-bind")
  expect_equal(w3@processing[[1]]$output[[1]]$metadata$test, "value")
})

test_that("noClick method on TaggedWave objects works", {
  w1 <- sine(10.3, samp.rate=450)
  w1 <- tagWave(w1)

  w2 <- concat(w1, w1, method="noClick")
  expect_true(is(w2, "TaggedWave"))
  expect_lt(length(w2@left), length(w1@left)*2)

  w3 <- concat(w1, w1, w1, method="noClick")
  expect_true(is(w3, "TaggedWave"))
  expect_lt(length(w3@left), length(w1@left)*3)
})

test_that("bind method on TaggedWaveMC objects works", {
  w1 <- sine(440, samp.rate=44100)
  w2 <- sine(440, samp.rate=44100)
  w1 <- tuneR::WaveMC(w1)
  w2 <- tuneR::WaveMC(w2)
  w1 <- tagWave(w1)
  w2 <- tagWave(w2)
  w2@metadata <- list("test" = "value")

  w3 <- concat(w1, w2, method="bind")
  expect_true(is(w3, "TaggedWaveMC"))
  expect_equal(nrow(w3@.Data), nrow(w1@.Data) + nrow(w2@.Data))
  expect_equal(w3@processing[[1]]$process, "concat-bind")
  expect_equal(w3@processing[[1]]$output[[1]]$metadata$test, "value")
})

test_that("noClick method on TaggedWaveMC objects works", {
  w1 <- tuneR::WaveMC(sine(10.3, samp.rate=450))
  w1 <- tagWave(w1)

  w2 <- concat(w1, w1, method="noClick")
  expect_true(is(w2, "TaggedWaveMC"))
  expect_lt(nrow(w2@.Data), nrow(w1@.Data)*2)

  w3 <- concat(w1, w1, w1, method="noClick")
  expect_true(is(w3, "TaggedWaveMC"))
  expect_lt(nrow(w3@.Data), nrow(w1@.Data)*3)
})
