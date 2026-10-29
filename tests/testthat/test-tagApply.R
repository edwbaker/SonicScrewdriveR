test_that("tagApply records a call on a TaggedWave", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  tw <- tagWave(w, origin = "test")
  tw@metadata <- list(site = "here")
  res <- tagApply(tw, tuneR::normalize, unit = "1")

  expect_s4_class(res, "TaggedWave")
  expect_equal(length(res@processing), 1)
  rec <- res@processing[[1]]
  expect_equal(rec$process, "tuneR::normalize")
  expect_equal(rec$func, "tuneR::normalize")
  expect_equal(rec$params, list(unit = "1"))
  expect_s3_class(rec$time, "POSIXct")
  expect_gte(rec$duration, 0)
  expect_equal(rec$input_hash, .waveHash(tw))
  expect_equal(rec$output_hash, .waveHash(res))
  expect_false(rec$input_hash == rec$output_hash)
  expect_equal(res@origin, "test")
  expect_equal(res@metadata$site, "here")
})

test_that("tagApply appends to an existing history and takes a label", {
  tw <- tagWave(tuneR::sine(440, duration = 4410, samp.rate = 44100))
  tw <- addProcess(tw, "earlier")
  res <- tagApply(tw, "normalize", unit = "1", label = "scale to 16 bit")
  expect_equal(length(res@processing), 2)
  expect_equal(res@processing[[1]]$process, "earlier")
  expect_equal(res@processing[[2]]$process, "scale to 16 bit")
})

test_that("tagApply does not record on untagged waves", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  res <- tagApply(w, tuneR::normalize, unit = "1")
  expect_false(.isTagged(res))
  expect_equal(res, tuneR::normalize(w, unit = "1"))
})

test_that("tagApply works on lists and WaveMC", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  res <- tagApply(list(tagWave(w), tagWave(w)), tuneR::normalize, unit = "1")
  expect_length(res, 2)
  expect_equal(res[[2]]@processing[[1]]$func, "tuneR::normalize")

  wmc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate = 44100, bit = 16)
  res <- tagApply(tagWave(wmc), tuneR::normalize, unit = "1")
  expect_s4_class(res, "TaggedWaveMC")
  expect_equal(length(res@processing), 1)
})

test_that("tagApply summarises waves in params and handles non-wave results", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  tw <- tagWave(w)
  res <- tagApply(tw, function(x, other) x + other, other = w)
  expect_match(res@processing[[1]]$params$other, "^<wave sha256:[0-9a-f]{64}>$")

  expect_equal(as.numeric(tagApply(tw, function(x) length(x@left))), length(w@left))
  expect_error(tagApply(1, identity), "Can only record")
})

test_that(".waveHash depends on samples and rate only", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  expect_equal(.waveHash(w), .waveHash(tagWave(w, origin = "x")))
  expect_false(.waveHash(w) == .waveHash(tuneR::sine(441, duration = 4410, samp.rate = 44100)))
})

test_that("tagApply attaches the history to a non-wave result", {
  tw <- tagWave(tuneR::sine(440, duration = 4410, samp.rate = 44100))
  tw <- addProcess(tw, "earlier")
  res <- tagApply(tw, function(x) mean(x@left), label = "mean level")

  expect_equal(as.numeric(res), mean(tw@left))
  hist <- attr(res, "processing")
  expect_equal(length(hist), 2)
  expect_equal(hist[[1]]$process, "earlier")
  expect_equal(hist[[2]]$process, "mean level")
  expect_equal(hist[[2]]$input_hash, .waveHash(tw))
  expect_null(hist[[2]]$output_hash)

  expect_null(tagApply(tw, function(x) NULL))
})

test_that("a process is recorded once, by the outermost call", {
  tw <- tagWave(tuneR::sine(440, duration = 4410, samp.rate = 44100))
  tw <- addProcess(tw, "earlier")

  # A function that records itself, which is how a function of this package
  # would use tagApply(): applied on its own it makes one record.
  quieten <- function(w, by = 2) {
    tagApply(w, function(x) {
      x@left <- x@left / by
      x
    }, label = "quieten")
  }
  res <- quieten(tw)
  expect_equal(length(res@processing), 2)
  rec <- res@processing[[2]]
  expect_equal(rec$process, "quieten")
  expect_equal(rec$input_hash, .waveHash(tw))
  expect_equal(rec$output_hash, .waveHash(res))

  # Wrapping it gives one record, under the outer call rather than two.
  res <- tagApply(tw, quieten, by = 4, label = "quieten a lot")
  expect_equal(length(res@processing), 2)
  expect_equal(res@processing[[2]]$process, "quieten a lot")
  expect_equal(res@processing[[2]]$func, "quieten")
  expect_equal(res@processing[[2]]$params$by, 4)

  # and recording is switched back on afterwards, even after an error
  expect_error(tagApply(tw, function(x) stop("no")), "no")
  res <- tagApply(tw, tuneR::normalize, unit = "1")
  expect_equal(length(res@processing), 2)
})

test_that("tagApply keeps the tags of tagged waves passed as arguments", {
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  tw <- tagWave(w)
  other <- tagWave(w, origin = "other")
  other@metadata <- list(site = "there")
  res <- tagApply(tw, function(x, y) x, y = other)
  expect_equal(res@processing[[1]]$params$y$origin, "other")
  expect_equal(res@processing[[1]]$params$y$metadata$site, "there")
  expect_match(res@processing[[1]]$params$y$sha256, "^[0-9a-f]{64}$")
})

test_that("anonymous functions are named as such", {
  tw <- tagWave(tuneR::sine(440, duration = 4410, samp.rate = 44100))
  expect_equal(tagApply(tw, function(x) x)@processing[[1]]$func, "<anonymous function>")
  expect_equal(tagApply(tw, base::identity)@processing[[1]]$func, "base::identity")
  f <- identity
  expect_equal(tagApply(tw, f)@processing[[1]]$func, "f")
})

test_that("tagApply works with a cluster", {
  skip_on_os("windows")
  cl <- parallel::makeForkCluster(2)
  on.exit(parallel::stopCluster(cl))
  w <- tuneR::sine(440, duration = 4410, samp.rate = 44100)
  res <- tagApply(list(tagWave(w), tagWave(w), tagWave(w)), tuneR::normalize, unit = "1", cl = cl)
  expect_length(res, 3)
  expect_equal(res[[3]]@processing[[1]]$func, "tuneR::normalize")
})

test_that(".needsUntag is TRUE for functions from other packages, including base", {
  expect_true(.needsUntag(seewave::ffilter))
  expect_true(.needsUntag(tuneR::normalize))
  expect_false(.needsUntag(generateNoise))
  user_fun <- function(x) x
  environment(user_fun) <- globalenv()
  expect_false(.needsUntag(user_fun))
  expect_true(.needsUntag(base::identity))
  expect_false(.needsUntag(sum))
})

test_that("tagApply lets seewave functions be used on tagged waves", {
  tw <- tagWave(noise("white", duration = 4410, samp.rate = 44100))
  tw@metadata <- list(site = "here")

  # returns a wave: the tags are kept and the call is recorded
  res <- tagApply(tw, seewave::ffilter, from = 1000, to = 2000, output = "Wave")
  expect_s4_class(res, "TaggedWave")
  expect_equal(res@metadata$site, "here")
  expect_equal(res@processing[[1]]$func, "seewave::ffilter")
  expect_equal(res@processing[[1]]$params$from, 1000)

  # returns something else: the history is attached to it
  sp <- tagApply(tw, seewave::spec, plot = FALSE)
  expect_true(is.matrix(sp))
  expect_equal(attr(sp, "processing")[[1]]$func, "seewave::spec")

  # without untagging seewave rejects the tagged wave
  expect_error(tagApply(tw, seewave::ffilter, from = 1000, to = 2000, output = "Wave", untag = FALSE))
  # and untag = TRUE can be used for a function of one's own that needs it
  res <- tagApply(tw, function(x) {stopifnot(!.isTagged(x)); x}, untag = TRUE)
  expect_s4_class(res, "TaggedWave")
})

test_that("tagApply handles tagged multi-channel waves with seewave", {
  w <- noise("white", duration = 4410, samp.rate = 44100)
  wmc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate = 44100, bit = 16)
  # ffilter returns the filtered channel as a Wave, so that is what is tagged
  res <- tagApply(tagWave(wmc), seewave::ffilter, from = 1000, to = 2000, output = "Wave")
  expect_s4_class(res, "TaggedWave")
  expect_equal(length(res@processing), 1)
  expect_equal(res@processing[[1]]$func, "seewave::ffilter")
})
