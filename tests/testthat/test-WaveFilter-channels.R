mono <- tuneR::noise("white", duration=4410, samp.rate=44100)
tone <- tuneR::sine(1500, duration=4410, samp.rate=44100)
#The noise and the tone from tuneR are floating point, and declared as 16 bit
#PCM their filtered samples would be rounded to whole numbers.
stereo <- tuneR::Wave(left=mono@left, right=tone@left, samp.rate=44100, bit=32, pcm=FALSE)
multi <- tuneR::WaveMC(data=cbind(mono@left, tone@left), samp.rate=44100, bit=32, pcm=FALSE)
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

test_that("filterWave returns whole samples where the input is integer PCM", {
  # seewave's output is floating point, and it was returned declaring the
  # input's integer PCM format, so the wave held samples its own format could
  # not: writeWave() rounded them with only its generic warning.
  set.seed(1)
  m <- tuneR::Wave(round(runif(8000, -10000, 10000)), samp.rate=8000, bit=16)
  s <- tuneR::Wave(m@left, right=rev(m@left), samp.rate=8000, bit=16)
  mc <- tuneR::WaveMC(cbind(m@left, rev(m@left)), samp.rate=8000, bit=16)
  f <- bandpass(from=500, to=2000)

  fm <- filterWave(m, f)
  expect_true(all(fm@left == round(fm@left)))
  fs <- filterWave(s, f)
  expect_true(all(fs@left == round(fs@left)))
  expect_true(all(fs@right == round(fs@right)))
  fmc <- filterWave(mc, f)
  expect_true(all(fmc@.Data == round(fmc@.Data)))
})

test_that("filterWave returns a wave in the format it was given", {
  # seewave returns 16 bit PCM whatever it is given, and a mono wave was
  # returned as seewave left it, so a floating point wave came back declaring
  # integer PCM.
  set.seed(1)
  formats <- list(
    list(bit=8, pcm=TRUE, x=round(runif(8000, 28, 228))),
    list(bit=16, pcm=TRUE, x=round(runif(8000, -10000, 10000))),
    list(bit=24, pcm=TRUE, x=round(runif(8000, -2e6, 2e6))),
    list(bit=32, pcm=TRUE, x=round(runif(8000, -5e8, 5e8))),
    list(bit=32, pcm=FALSE, x=runif(8000, -0.5, 0.5)),
    list(bit=64, pcm=FALSE, x=runif(8000, -0.5, 0.5))
  )
  for (format in formats) {
    w <- tuneR::Wave(format$x, samp.rate=8000, bit=format$bit, pcm=format$pcm)
    filtered <- filterWave(w, bandpass(from=500, to=2000))
    expect_equal(filtered@bit, format$bit)
    expect_equal(filtered@pcm, format$pcm)
  }

  # A floating point wave is not rounded.
  float <- tuneR::Wave(runif(8000, -0.5, 0.5), samp.rate=8000, bit=32, pcm=FALSE)
  filtered <- filterWave(float, bandpass(from=500, to=2000))
  expect_false(all(filtered@left == round(filtered@left)))
})

test_that("bandpass keeps the level of what it passes", {
  # seewave's ffilter() returns what it passes at one part in its window length
  # of the level it had. A full scale tone came back with a peak of 32, and a
  # recording peaking at -30 dBFS was nothing but zeros once rounded.
  times <- seq_len(16000) / 8000
  tone <- tuneR::Wave(round(20000 * sin(2 * pi * 1000 * times)), samp.rate=8000, bit=16)
  middle <- 4000:12000
  filtered <- filterWave(tone, bandpass(from=500, to=2000))
  expect_equal(max(abs(filtered@left[middle])), 20000, tolerance=0.01)

  # The output is scaled back by the window length it was filtered with.
  short <- filterWave(tone, bandpass(from=500, to=2000, wl=256))
  expect_equal(max(abs(short@left[middle])), 20000, tolerance=0.01)

  # Asked to rescale, ffilter puts its output in the range of the input itself,
  # and it is not scaled again.
  rescaled <- filterWave(tone, bandpass(from=500, to=2000, rescale=TRUE))
  expect_equal(range(rescaled@left), range(tone@left))

  set.seed(2)
  quiet <- tuneR::Wave(round(rnorm(8000, sd=250)), samp.rate=8000, bit=16)
  expect_gt(stats::sd(filterWave(quiet, bandpass(from=500, to=2000))@left), 100)
})

test_that("eight bit audio is filtered about its midpoint", {
  # Eight bit PCM is unsigned about 128. A band pass removes that offset with
  # the rest of the lowest frequencies, which left the filtered samples either
  # side of zero in a format running from 0 to 255.
  times <- seq_len(16000) / 8000
  w <- tuneR::Wave(round(128 + 100 * sin(2 * pi * 1000 * times)), samp.rate=8000, bit=8)
  middle <- 4000:12000
  filtered <- filterWave(w, bandpass(from=500, to=2000))
  expect_equal(filtered@bit, 8)
  expect_equal(mean(filtered@left[middle]), 128, tolerance=0.01)
  expect_equal(max(filtered@left[middle]) - 128, 100, tolerance=0.02)
  expect_true(all(filtered@left >= 0 & filtered@left <= 255))
})

test_that("a filtered WaveMC keeps the names of its channels", {
  # The channels were bound without their names, and writeWave() builds the
  # channel mask of the file from them.
  set.seed(1)
  mc <- tuneR::WaveMC(
    cbind(FL=round(runif(8000, -1e4, 1e4)), FR=round(runif(8000, -1e4, 1e4))),
    samp.rate=8000, bit=16
  )
  expect_equal(colnames(filterWave(mc, bandpass(from=500, to=2000))@.Data), c("FL", "FR"))
  expect_equal(colnames(autoBandPass(mc)@.Data), c("FL", "FR"))

  path <- withr::local_tempfile(fileext=".wav")
  tuneR::writeWave(filterWave(mc, bandpass(from=500, to=2000)), path)
  expect_equal(colnames(tuneR::readWave(path, toWaveMC=TRUE)@.Data), c("FL", "FR"))

  # A WaveMC whose channels have no names still has none.
  unnamed <- tuneR::WaveMC(unname(mc@.Data), samp.rate=8000, bit=16)
  expect_null(colnames(filterWave(unnamed, bandpass(from=500, to=2000))@.Data))
})

test_that("filterWave carries the tags across", {
  w <- tagWave(mono, origin="test")
  w@metadata <- list(site="a")
  r <- filterWave(w, filt)
  expect_equal(r@origin, "test")
  expect_equal(r@metadata, list(site="a"))
})
