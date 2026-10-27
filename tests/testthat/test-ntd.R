#A run of pulses to take intervals between. The samples themselves are not read
#by the functions under test, only their count, so a plain ramp will do.
ntdWave <- function(n=1000, samp.rate=1000) {
  tuneR::Wave(round(seq_len(n) %% 100), samp.rate=samp.rate, bit=16)
}

test_that("fewer than two events is an error, not an NA interval", {
  wave <- ntdWave()
  expect_error(ntd(wave, integer(0), function(wave) 1), "at least two events")
  expect_error(ntd(wave, 1, function(wave) 1), "at least two events")
})

test_that("one interval is returned for each pair of consecutive events", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201, 301), function(wave) length(wave@left))
  expect_equal(nrow(res), 3)
  expect_equal(colnames(res), c("k", "Fk"))
})

test_that("the natural time k runs from 1/N to 1", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201, 301), function(wave) length(wave@left))
  expect_equal(res[, "k"], c(1/3, 2/3, 1))
})

test_that("each interval is passed as the wave between one event and the next", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left))
  #Both endpoints are included, so an interval of 100 samples gives 101.
  expect_equal(res[, "Fk"], c(101, 201))
})

test_that("the interval wave keeps the sample rate and bit depth of the input", {
  wave <- ntdWave(samp.rate=8000)
  seen <- ntd(wave, c(1, 101, 201), function(wave) wave@samp.rate)
  expect_equal(unname(seen[, "Fk"]), c(8000, 8000))
  seen <- ntd(wave, c(1, 101, 201), function(wave) wave@bit)
  expect_equal(unname(seen[, "Fk"]), c(16, 16))
})

test_that("argument='vector' hands the function the samples rather than the wave", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201), function(x) sum(x), argument="vector")
  expected <- c(sum(wave@left[1:101]), sum(wave@left[101:201]))
  expect_equal(unname(res[, "Fk"]), expected)
})

test_that("additional arguments reach the applied function", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201), function(wave, add) length(wave@left) + add, add=5)
  expect_equal(unname(res[, "Fk"]), c(106, 106))
})

test_that("normalise=TRUE turns the outputs into a probability density", {
  wave <- ntdWave()
  raw <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left))
  dens <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left), normalise=TRUE)
  expect_equal(sum(dens[, "Fk"]), 1)
  expect_equal(dens[, "Fk"], raw[, "Fk"] / sum(raw[, "Fk"]))
})
