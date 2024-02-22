test_that("single channel file works", {
  # Basic function
  w <- tuneR::sine(440, duration=44100, samp.rate=44100)
  f1 <- function(w, channel) {
    return(list("channel" = channel))
  }
  expect_equal(allChannels(w, f1), list("channel" = 1))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus"), list("octopus" = 1))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3), list(1))

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
  expect_equal(allChannels(w, f1, cl=cl), list("channel" = 1))

  # Function with non-standard channel.param
  f2 <- function(w, octopus) {
    return(list("octopus" = octopus))
  }
  expect_equal(allChannels(w, f2, channel.param="octopus", cl=cl), list("octopus" = 1))

  # Function not returning a list
  f3 <- function(w, channel) {
    return(channel)
  }
  expect_equal(allChannels(w, f3, cl=cl), list(1))

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
