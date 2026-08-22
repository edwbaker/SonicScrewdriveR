test_that("scaleRGB scales to the full range", {
  expect_equal(scaleRGB(c(0, 5, 10)), c(0L, 127L, 255L))
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
