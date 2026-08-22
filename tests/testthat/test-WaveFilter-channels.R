mono <- tuneR::noise("white", duration=4410, samp.rate=44100)
tone <- tuneR::sine(1500, duration=4410, samp.rate=44100)
stereo <- tuneR::Wave(left=mono@left, right=tone@left, samp.rate=44100, bit=16)
multi <- tuneR::WaveMC(data=cbind(mono@left, tone@left), samp.rate=44100, bit=16)
filt <- bandpass(from=1000, to=2000)

test_that("filterWave keeps every channel", {
  # The filters read the left slot, so a multi-channel wave came back with only
  # its first channel and no indication that the rest had gone.
  s <- filterWave(stereo, filt)
  expect_true(s@stereo)
  expect_false(identical(s@left, s@right))

  m <- filterWave(multi, filt)
  expect_s4_class(m, "WaveMC")
  expect_equal(ncol(m@.Data), 2)
  expect_false(identical(m@.Data[,1], m@.Data[,2]))
  # Every channel is filtered, not just the first.
  expect_false(identical(as.numeric(m@.Data[,2]), as.numeric(multi@.Data[,2])))
})

test_that("filterWave works on tagged waves", {
  # seewave rejects a TaggedWave, and addProcess has no method for the plain Wave
  # a filter gives back, so this used to raise an error either way.
  t1 <- filterWave(tagWave(mono), filt)
  expect_s4_class(t1, "TaggedWave")
  expect_equal(length(t1@processing), 1)

  expect_s4_class(filterWave(tagWave(stereo), filt), "TaggedWave")
  expect_s4_class(filterWave(tagWave(multi), filt), "TaggedWaveMC")
})

test_that("filterWave records each filter it applies", {
  # addProcess nested the history rather than appending to it, so its shape
  # changed with every call.
  w <- filterWave(filterWave(tagWave(mono), filt), bandpass(from=1200, to=1800))
  expect_equal(length(w@processing), 2)
  expect_true(all(vapply(w@processing, function(x) "process" %in% names(x), logical(1))))
})

test_that("filterWave carries the tags across", {
  w <- tagWave(mono, origin="test")
  w@metadata <- list(site="a")
  r <- filterWave(w, filt)
  expect_equal(r@origin, "test")
  expect_equal(r@metadata, list(site="a"))
})
