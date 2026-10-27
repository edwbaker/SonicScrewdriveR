# Each operation must give the same wave whether or not it is tagged, leave plain
# Wave/WaveMC objects untagged, and record itself on tagged ones.

.ops_wave <- function() {
  set.seed(1)
  tuneR::Wave(round(runif(4410, -1000, 1000)), samp.rate = 44100, bit = 16)
}
.ops_wavemc <- function() {
  w <- .ops_wave()
  tuneR::WaveMC(cbind(w@left, rev(w@left)), samp.rate = 44100, bit = 16)
}
.samples <- function(w) if (inherits(w, "WaveMC")) w@.Data else cbind(w@left, w@right)

.expect_parity <- function(op, name, w, ...) {
  plain <- op(w, ...)
  tagged_in <- tagWave(w, origin = "test")
  tagged_in@metadata <- list(site = "here")
  tagged <- op(tagged_in, ...)

  expect_false(.isTagged(plain))
  expect_true(.isTagged(tagged))
  expect_equal(.samples(untagWave(tagged)), .samples(plain))
  expect_equal(tagged@origin, "test")
  expect_equal(tagged@metadata$site, "here")
  rec <- tagged@processing[[length(tagged@processing)]]
  expect_equal(rec$func, name)
  expect_equal(rec$input_hash, .waveHash(tagged_in))
  expect_equal(rec$output_hash, .waveHash(tagged))
  expect_equal(length(tagged@processing), 1)
}

for (kind in c("Wave", "WaveMC")) {
  make <- if (kind == "Wave") .ops_wave else .ops_wavemc

  test_that(paste("normalise gives the same result tagged and untagged:", kind), {
    .expect_parity(normalise, "normalise", make(), unit = "1")
  })
  test_that(paste("cutws gives the same result tagged and untagged:", kind), {
    .expect_parity(cutws, "cutws", make(), from = 10, to = 200)
  })
  test_that(paste("generateTimeMask gives the same result tagged and untagged:", kind), {
    .expect_parity(generateTimeMask, "generateTimeMask", make(), dutyCycle = 0.5)
  })
}

test_that("upsample gives the same result tagged and untagged", {
  .expect_parity(upsample, "upsample", .ops_wave(), upsample.rate = 88200)
  stereo <- tuneR::stereo(.ops_wave(), .ops_wave())
  .expect_parity(upsample, "upsample", stereo, upsample.rate = 88200)
})

test_that("generateTimeShift records each shifted wave", {
  for (kind in c("Wave", "WaveMC")) {
    w <- if (kind == "Wave") .ops_wave() else .ops_wavemc()
    tw <- tagWave(w, origin = "test")
    for (type in c("silent", "rotate")) {
      plain <- generateTimeShift(w, type = type, amount = c(0.01, 0.02), where = "both")
      tagged <- generateTimeShift(tw, type = type, amount = c(0.01, 0.02), where = "both")
      expect_equal(length(tagged), length(plain))
      for (i in seq_along(plain)) {
        expect_false(.isTagged(plain[[i]]))
        expect_true(.isTagged(tagged[[i]]))
        expect_equal(.samples(untagWave(tagged[[i]])), .samples(plain[[i]]))
        expect_equal(length(tagged[[i]]@processing), 1)
        expect_equal(tagged[[i]]@processing[[1]]$func, "generateTimeShift")
      }
    }
  }
})

test_that("generateTimeMask on a list passes all its arguments on", {
  w <- .ops_wave()
  res <- generateTimeMask(list(w, w), dutyCycle = 0.5)
  expect_equal(res[[1]], generateTimeMask(w, dutyCycle = 0.5))
})

test_that("autoBandPass works on tagged and untagged waves", {
  skip_if_not_installed("seewave")
  w <- tuneR::sine(3000, duration = 44100, samp.rate = 44100)
  plain <- autoBandPass(w, n.bw = 1L)
  tagged <- autoBandPass(tagWave(w), n.bw = 1L)
  expect_false(.isTagged(plain))
  expect_true(.isTagged(tagged))
  expect_equal(tagged@processing[[1]]$func, "autoBandPass")
  expect_equal(.samples(untagWave(tagged)), .samples(plain))
})

test_that("operations still reject things that are not waves", {
  expect_error(cutws(1:10))
  expect_error(upsample(1:10, 2))
  expect_error(normalise(1:10))
})

test_that("operations inside tagApply are recorded once", {
  tw <- tagWave(.ops_wave())
  res <- tagApply(tw, cutws, from = 5, to = 50)
  expect_equal(length(res@processing), 1)
  expect_equal(res@processing[[1]]$func, "cutws")
})
