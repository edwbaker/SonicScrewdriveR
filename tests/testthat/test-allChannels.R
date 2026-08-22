test_that("single channel file works", {
  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1), list(list("channel" = 1)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus"), list(list("octopus" = 1)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3), list(list(1)))

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- makeForkCluster(2, outfile="")

  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1, cl=cl), list(list("channel" = 1)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus", cl=cl), list(list("octopus" = 1)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3, cl=cl), list(list(1)))

  parallel::stopCluster(cl)
})

test_that("stereo channel file works", {
  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1), list(list("channel" = 1), list("channel" = 2)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus"), list(list("octopus" = 1), list("octopus" = 2)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3), list(list(1), list(2)))

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- makeForkCluster(2, outfile="")

  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1, cl=cl), list(list("channel" = 1), list("channel" = 2)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus", cl=cl), list(list("octopus" = 1), list("octopus" = 2)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3, cl=cl), list(list(1), list(2)))

  parallel::stopCluster(cl)

})

test_that("WaveMC file works", {
  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  w <- tuneR::WaveMC(w)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1), list(list("channel" = 1), list("channel" = 2)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus"), list(list("octopus" = 1), list("octopus" = 2)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3), list(list(1), list(2)))

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- makeForkCluster(2, outfile="")

  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1, cl=cl), list(list("channel" = 1), list("channel" = 2)))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus", cl=cl), list(list("octopus" = 1), list("octopus" = 2)))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3, cl=cl), list(list(1), list(2)))

  parallel::stopCluster(cl)

})

test_that("output.FUN param works with soundecology example", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::bioacoustic_index, channel.param=NULL, output.FUN = channels_se)
  expect_equal(t, list(list(10.9717422), list(10.9717422)))

  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::acoustic_evenness, channel.param=NULL, output.FUN = channels_se)
  expect_equal(t, list(list(0.9), list(0.9)))

  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::acoustic_diversity, channel.param=NULL, output.FUN = channels_se)
  expect_equal(t, list(list(0), list(0)))

  # Don't run on Windoze
  if (.Platform$OS.type == "windows") {
    return()
  }
  cl <- makeForkCluster(2, outfile="")

  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::bioacoustic_index, channel.param=NULL, output.FUN = channels_se, cl=cl)
  expect_equal(t, list(list(10.9717422), list(10.9717422)))

  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::acoustic_evenness, channel.param=NULL, output.FUN = channels_se, cl=cl)
  expect_equal(t, list(list(0.9), list(0.9)))

  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)
  t <- allChannels(w, soundecology::acoustic_diversity, channel.param=NULL, output.FUN = channels_se, cl=cl)
  expect_equal(t, list(list(0), list(0)))

  parallel::stopCluster(cl)
})

test_that("output structure does not depend on class or channel count", {
  w <- tuneR::sine(440, duration=4410, samp.rate=44100)
  f <- function(w, channel) channel

  mono <- w
  stereo <- tuneR::stereo(w, w)
  mc1 <- tuneR::WaveMC(cbind(w@left), samp.rate=44100, bit=16)
  mc3 <- tuneR::WaveMC(cbind(w@left, w@left, w@left), samp.rate=44100, bit=16)

  # One entry per channel, each entry a list, in every case.
  expect_equal(allChannels(mono, f), list(list(1)))
  expect_equal(allChannels(mc1, f), list(list(1)))
  expect_equal(allChannels(stereo, f), list(list(1), list(2)))
  expect_equal(allChannels(mc3, f), list(list(1), list(2), list(3)))

  # A mono Wave and a single channel WaveMC are indistinguishable.
  expect_equal(allChannels(mono, f), allChannels(mc1, f))
})

test_that("a single channel function can be applied to any wave", {
  sr <- 8000
  n <- 4000
  L <- tuneR::Wave(round(sin(2*pi*300*(1:n)/sr) * 20000), samp.rate=sr, bit=16)
  R <- tuneR::Wave(round(sin(2*pi*1500*(1:n)/sr) * 3000), samp.rate=sr, bit=16)
  mc <- tuneR::WaveMC(cbind(L@left, R@left), samp.rate=sr, bit=16)

  # WaveMC channels are extracted as Wave objects, so functions that read the
  # left slot work on them.
  expect_equal(
    allChannels(mc, dutyCycle, channel.param=NULL),
    list(list(dutyCycle(L)), list(dutyCycle(R)))
  )
  expect_equal(
    allChannels(tuneR::stereo(L, R), dutyCycle, channel.param=NULL),
    allChannels(mc, dutyCycle, channel.param=NULL)
  )
})

test_that("channels_se handles every soundecology index that names a left channel", {
  w <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)

  # Previously ndsi returned NULL for every channel, as its value was not looked for.
  invisible(capture.output(
    expected <- soundecology::ndsi(tuneR::channel(w, "left"))$ndsi_left
  ))
  invisible(capture.output(
    t <- allChannels(w, soundecology::ndsi, channel.param=NULL, output.FUN=channels_se)
  ))
  expect_equal(t, list(list(expected), list(expected)))

  # The names each index function uses, checked without calling soundecology.
  expect_equal(channels_se(left_area=1, right_area=2), list(1))
  expect_equal(channels_se(adi_left=1, adi_right=2), list(1))
  expect_equal(channels_se(aei_left=1, aei_right=2), list(1))
  expect_equal(channels_se(ndsi_left=1, ndsi_right=2), list(1))
  expect_equal(channels_se(AciTotAll_left=1, AciTotAll_right=2), list(1))
})

test_that("channels_se reports output it does not recognise", {
  # Previously returned NULL, so a mistake showed up as an empty result much later.
  expect_error(channels_se(some_other_index=1), "supported soundecology index")
})
