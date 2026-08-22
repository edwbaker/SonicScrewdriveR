#' Build a multichannel wave holding two pulses, reaching each channel at a
#' different time.
#'
#' @param delays Delay of each channel in samples, relative to the first.
#' @return A WaveMC object.
#' @noRd
pulsedWave <- function(delays, n=8000, samp.rate=8000) {
  t <- seq_len(n + 800)
  envelope <- exp(-((t-2000)^2)/(2*100^2)) + exp(-((t-6000)^2)/(2*100^2))
  signal <- envelope * sin(2*pi*1000*t/samp.rate)
  data <- vapply(delays, function(d) signal[(400 - d) + seq_len(n)], numeric(n))
  return(tuneR::WaveMC(data * 1e4, samp.rate=samp.rate, bit=32, pcm=FALSE))
}

test_that("corWaveMC returns a correlation and a delay for every channel", {
  w <- pulsedWave(c(0, -40, 25))
  r <- corWaveMC(w, times=0.25, window=0.4, method="pearson")

  expect_length(r, 1)
  expect_equal(names(r[[1]]), c("correlations", "delays"))
  expect_length(r[[1]]$correlations, 3)
  expect_length(r[[1]]$delays, 3)
  #Every entry is what corenv() returns.
  expect_true(all(vapply(r[[1]]$correlations, function(x) all(c("r", "rmax", "p", "t") %in% names(x)), logical(1))))
  expect_equal(vapply(r[[1]]$correlations, function(x) x$rmax, numeric(1)), rep(1, 3))
})

test_that("corWaveMC measures delays in the same direction as tdoa", {
  w <- pulsedWave(c(0, -40, 25))
  r <- corWaveMC(w, times=0.25, window=0.4, method="pearson")

  #corenv() reports a delay of zero or less as one sample greater than it is,
  #so only the channel the sound reached last is exact.
  expect_equal(r[[1]]$delays * 8000, c(1, -39, 25))
  #The sign is the opposite of the offset corenv() reports.
  expect_equal(r[[1]]$delays, -vapply(r[[1]]$correlations, function(x) x$t, numeric(1)))
  #Which is the direction tdoa() measures in.
  expect_equal(sign(r[[1]]$delays[2:3]), sign(tdoa(w, method="envelope")$samples[2:3]))
})

test_that("corWaveMC correlates each event asked for", {
  w <- pulsedWave(c(0, -40, 25))
  r <- corWaveMC(w, times=c(0.25, 0.75), window=0.4, method="pearson")

  expect_length(r, 2)
  expect_equal(r[[1]]$delays, r[[2]]$delays)
})

test_that("corWaveMC passes arguments on to corenv", {
  w <- pulsedWave(c(0, -40, 25))
  #The rank correlation corenv() uses by default finds no offset at all in a
  #recording that is silent between its events.
  spearman <- corWaveMC(w, times=0.25, window=0.4)
  expect_equal(spearman[[1]]$delays * 8000, c(1, 1, 1))
  expect_lt(spearman[[1]]$correlations[[2]]$rmax, 1)
})

test_that("corWaveMC needs a WaveMC object", {
  expect_error(corWaveMC(tuneR::sine(440), times=0.1, window=0.05), "Expecting a WaveMC object")
})
