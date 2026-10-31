test_that("scaleRGB scales to the full range", {
  expect_equal(scaleRGB(c(0, 5, 10)), c(0L, 128L, 255L))
  # A vector with no variation has no differences to show.
  expect_equal(scaleRGB(rep.int(3, 4)), rep.int(255, 4))
  expect_equal(scaleRGB(rep.int(3, 4), no.diff=0), rep.int(0, 4))
})

test_that("map2RGB combines three channels", {
  expect_equal(
    map2RGB(c(0, 10), c(10, 0), c(0, 10)),
    c("#00FF00", "#FF00FF")
  )
  # A channel with no variation is fully saturated rather than absent, following
  # the no.diff argument of scaleRGB().
  expect_equal(map2RGB(c(0, 10), c(10, 0), c(0, 0)), c("#00FFFF", "#FF00FF"))
})

test_that("fcisIndexNames lists the available indices", {
  expect_true(all(c("power", "ACI", "entropy", "background", "cover") %in% fcisIndexNames()))
})

test_that("per bin indices behave as expected", {
  # A bin whose amplitude never changes has no complexity and maximal entropy.
  steady <- matrix(rep.int(1, 40), nrow=4)
  expect_equal(sonicscrewdriver:::.fcisIndexFunctions$ACI(steady), rep.int(0, 4))
  expect_equal(sonicscrewdriver:::.fcisIndexFunctions$entropy(steady), rep.int(1, 4))
  expect_equal(sonicscrewdriver:::.fcisIndexFunctions$power(steady), rep.int(1, 4))

  # A silent bin divides by zero, which must not propagate.
  silent <- matrix(0, nrow=2, ncol=10)
  for (i in fcisIndexNames()) {
    v <- sonicscrewdriver:::.fcisIndexFunctions[[i]](silent)
    expect_true(all(is.finite(v)), info=i)
  }

  # A bin that alternates is more complex than one that does not.
  alternating <- matrix(rep(c(1, 5), 10), nrow=2, byrow=TRUE)
  expect_true(all(sonicscrewdriver:::.fcisIndexFunctions$ACI(alternating) > 0))
  expect_true(all(sonicscrewdriver:::.fcisIndexFunctions$entropy(alternating) < 1))
})

test_that("fcis returns one column per window", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))

  f <- fcis(w, window.length=w@samp.rate, wl=128)
  expect_s3_class(f, "fcis")
  expect_equal(ncol(f$colours), floor(length(w) / w@samp.rate))
  expect_equal(nrow(f$colours), length(f$freq))
  expect_equal(f$channels, c("power", "ACI", "entropy"))
  expect_equal(f$window.seconds, 1)
  expect_true(all(grepl("^#[0-9A-F]{6}$", f$colours)))

  # Every index requested is returned, with the same shape as the colours.
  expect_equal(names(f$indices), c("power", "ACI", "entropy"))
  for (i in f$indices) expect_equal(dim(i), dim(f$colours))
})

test_that("fcis accepts any three of the available indices", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))

  f <- fcis(w, window.length=w@samp.rate, wl=128, indices=c("background", "ACI", "cover"))
  expect_equal(f$channels, c("background", "ACI", "cover"))
})

test_that("fcis rejects the wrong number of indices or unknown ones", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))

  expect_error(fcis(w, indices=c("power", "ACI")), "Three indices are required")
  expect_error(fcis(w, window.length=w@samp.rate, indices=c("power", "ACI", "sonic")), "Unknown index")
})

test_that("fcis works from a filename", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- tuneR::readWave(f)

  from.file <- fcis(f, window.length=w@samp.rate, wl=128)
  from.wave <- fcis(w, window.length=w@samp.rate, wl=128)
  expect_equal(dim(from.file$colours), dim(from.wave$colours))
})

test_that("fcis can be plotted", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  f <- fcis(w, window.length=w@samp.rate, wl=128)

  expect_silent(plot(f))
  expect_null(plot(f))
})

test_that("the background index matches quantile(), including for very quiet bins", {
  set.seed(1)
  # A loud bin and a bin many orders of magnitude quieter, as happens in real
  # recordings. A faster whole-matrix sort would lose the quiet bin entirely.
  amp <- rbind(
    matrix(runif(200, 0, 1e3), nrow=1),
    matrix(runif(200, 0, 1e-18), nrow=1)
  )[rep(1:2, 8), ]

  expected <- apply(amp, 1, stats::quantile, probs=0.1, names=FALSE)
  actual <- sonicscrewdriver:::.fcisIndexFunctions$background(amp)

  expect_equal(actual, expected)
  # Checked per bin, as a relative comparison of the whole vector is dominated by
  # the loud bins and would not notice the quiet ones being zeroed.
  for (i in seq_along(expected)) {
    expect_equal(actual[i], expected[i], info=paste("bin", i))
  }
  expect_true(all(actual[c(2, 4, 6)] > 0))
})

test_that("the background of a single window is the window itself", {
  #One column has no distribution to take a quantile of.
  amp <- matrix(c(3, 7, 11), nrow=3)
  expect_equal(sonicscrewdriver:::.fcisBackground(amp), c(3, 7, 11))
  expect_equal(
    sonicscrewdriver:::.fcisIndexFunctions$background(amp),
    apply(amp, 1, stats::quantile, probs=0.1, names=FALSE)
  )
})

test_that("the background matches quantile() where the quantile falls on a window", {
  #Eleven columns put the tenth percentile exactly on the second lowest value,
  #so no interpolation between two values is needed.
  set.seed(7)
  amp <- matrix(runif(33), nrow=3)
  expect_equal(
    sonicscrewdriver:::.fcisBackground(amp),
    apply(amp, 1, stats::quantile, probs=0.1, names=FALSE)
  )
  #The same for a probability that does not land on a window.
  expect_equal(
    sonicscrewdriver:::.fcisBackground(amp, probs=0.25),
    apply(amp, 1, stats::quantile, probs=0.25, names=FALSE)
  )
})

test_that("no window length summarises the whole recording in one column", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  f <- fcis(w, window.length=NULL, wl=128)
  expect_s3_class(f, "fcis")
  expect_equal(ncol(f$colours), 1)
  expect_equal(nrow(f$colours), length(f$freq))
  #There is no window, so there is no length of one to report.
  expect_true(is.na(f$window.seconds))
})

test_that("a single column is plotted against windows rather than against time", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  f <- fcis(w, window.length=NULL, wl=128)
  expect_silent(plot(f))
  expect_null(plot(f))
  #A label given is still used.
  expect_silent(plot(f, xlab="Recording"))
})

test_that("several files are analysed as one spectrogram", {
  f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
  w <- tuneR::readWave(f)
  one <- fcis(f, window.length=w@samp.rate, wl=128)
  two <- fcis(c(f, f), window.length=w@samp.rate, wl=128)
  #The columns of both files, in the order the files were given.
  expect_equal(ncol(two$colours), 2*ncol(one$colours))
  expect_equal(nrow(two$colours), nrow(one$colours))
  expect_equal(two$window.seconds, one$window.seconds)
  expect_equal(two$freq, one$freq)
})

test_that("a window longer than the recording is reported rather than drawn empty", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  expect_error(
    fcis(w, window.length=length(w@left)*10, wl=128),
    "No complete windows to analyse."
  )
})

test_that("fcis gives the frequencies of its bins in Hz", {
  #seewave::spectro() gives them in kHz, which is how they were returned.
  samp.rate <- 8000
  times <- seq_len(samp.rate)/samp.rate
  tone <- tuneR::Wave(round(10000*sin(2*pi*1000*times)), samp.rate=samp.rate, bit=16)
  f <- fcis(tone, window.length=NULL, wl=128)

  spec <- seewave::spectro(tone, f=samp.rate, wl=128, plot=FALSE, dB=NULL, norm=FALSE)
  expect_equal(f$freq, spec$freq * 1000)
  #The bin holding the most power is the one the tone is in.
  expect_equal(f$freq[which.max(f$indices$power[, 1])], 1000)
  expect_lt(max(f$freq), samp.rate/2)
})

test_that("fcis is plotted against frequencies in Hz", {
  drawn <- NULL
  local_mocked_bindings(
    plot = function(x, ..., ylim, ylab) drawn <<- list(ylim=ylim, ylab=ylab),
    rasterImage = function(...) NULL
  )
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))
  f <- fcis(w, window.length=NULL, wl=128)
  #Called by name, as plot() is the stand-in here and would not dispatch.
  sonicscrewdriver:::plot.fcis(f)
  expect_equal(drawn$ylab, "Frequency (Hz)")
  expect_equal(drawn$ylim, c(0, max(f$freq)))
  expect_gt(max(f$freq), 1000)
})
test_that("scale transforms each channel before the colours are calculated", {
  w <- tuneR::readWave(system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver"))

  default <- fcis(w, window.length=w@samp.rate, wl=128)
  identity.scale <- fcis(w, window.length=w@samp.rate, wl=128, scale=function(x) x)
  expect_equal(default$colours, identity.scale$colours)

  clipped <- fcis(w, window.length=w@samp.rate, wl=128,
                  scale=list(fcisClip(log=TRUE), fcisClip(), fcisClip()))
  expect_equal(dim(clipped$colours), dim(default$colours))
  expect_false(identical(clipped$colours, default$colours))

  expect_error(fcis(w, window.length=w@samp.rate, wl=128, scale=list(log)),
               "three functions")
  expect_error(fcis(w, window.length=w@samp.rate, wl=128, scale=function(x) x[1]),
               "one value for each")
})

test_that("fcisClip limits values to the requested percentiles", {
  x <- matrix(c(-100, 1:98, 1000), nrow=10)

  clipped <- fcisClip(probs=c(0.1, 0.9))(x)
  expect_equal(dim(clipped), dim(x))
  expect_equal(min(clipped), unname(stats::quantile(x, 0.1)))
  expect_equal(max(clipped), unname(stats::quantile(x, 0.9)))

  # Values with no logarithm are treated as the lower limit, not dropped.
  logged <- fcisClip(log=TRUE)(matrix(c(0, 1, 10, 100), nrow=2))
  expect_false(anyNA(logged))

  expect_error(fcisClip(probs=c(0.9, 0.1)), "increasing")
})

test_that("a channel with nothing to show is left as it is", {
  # Where the percentiles meet there is no range to clip to, and clipping to a
  # single value would turn a flat channel into one of a colour it did not
  # earn. It is passed through for scaleRGB to recognise as having no
  # differences to show.
  flat <- matrix(5, nrow=4, ncol=3)
  expect_identical(fcisClip()(flat), flat)

  # The same where the middle of the data is flat but its tails are not
  mostly.flat <- matrix(c(-50, rep(7, 10), 90), nrow=3)
  expect_identical(fcisClip(probs=c(0.2, 0.8))(mostly.flat), mostly.flat)
})

test_that("scaleRGB rounds to the nearest level rather than truncating", {
  # The midpoint of the range scales to 127.5, which belongs at 128. Truncation
  # returned 127 and so biased every channel down by up to a whole level.
  expect_equal(scaleRGB(0:2), c(0L, 128L, 255L))

  # The endpoints are still exact, and nothing leaves the byte range.
  s <- scaleRGB(seq(0, 1, length.out = 9))
  expect_equal(s[1], 0L)
  expect_equal(s[9], 255L)
  expect_true(all(s >= 0L & s <= 255L))

  # Every value is now within half a level of the exact rescaling, which
  # truncation could not promise: it was short by up to a whole level.
  exact <- seq(0, 255, length.out = 9)
  expect_true(all(abs(s - exact) <= 0.5))
})
