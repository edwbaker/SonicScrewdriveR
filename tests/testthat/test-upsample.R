test_that("upsample rejects incorrect input", {
  wave <- tuneR::sine(4000, samp.rate=44100)

  expect_error(upsample(wave, 44200, method="basic"),"Scale factor is not an integer.")

})

test_that("upsample works as expected", {
  wave <- tuneR::sine(4000, samp.rate=44100)

  upsampled_wave <- upsample(wave, wave@samp.rate*3, method="basic")
  expect_equal(length(wave@left)*3, length(upsampled_wave@left))

  expect_equal(wave@samp.rate*3, upsampled_wave@samp.rate)
  expect_equal(wave@bit, upsampled_wave@bit)
  expect_equal(wave@pcm, upsampled_wave@pcm)

  stereo <- stereo(wave, wave)
  upsampled_stereo <- upsample(stereo, stereo@samp.rate*5, method="basic")
  expect_equal(length(stereo@left)*5, length(upsampled_stereo@left))
  expect_equal(length(stereo@right)*5, length(upsampled_stereo@right))

  expect_equal(stereo@samp.rate*5, upsampled_stereo@samp.rate)
  expect_equal(stereo@bit, upsampled_stereo@bit)
  expect_equal(stereo@pcm, upsampled_stereo@pcm)
})

# Reference implementation: the loop-based .upsampleChannel(), retained so the
# vectorised version can be checked against it.
ref_upsampleChannel <- function(channel, sf, method) {
  n <- length(channel)
  new <- rep.int(NA_real_, n*sf)
  for (i in 1:n) {
    new[sf*(i-1)+1] <- channel[i]
    if (method == "basic" && sf > 1) {
      d <- if (i < n) (channel[i+1] - channel[i]) / sf else 0
      for (j in 1:(sf-1)) {
        new[sf*(i-1)+1+j] <- channel[i] + j*d
      }
    }
  }
  return(new)
}

test_that("upsample matches the reference implementation", {
  set.seed(3)
  channels <- list(
    sine = sin(seq(0, 8*pi, length.out=200)) * 2^13,
    noise = runif(200, -2^13, 2^13),
    single = 1234,
    pair = c(-1000, 1000),
    flat = rep.int(500, 20)
  )

  for (nm in names(channels)) {
    for (sf in c(1, 2, 3, 5, 8)) {
      for (method in c("basic", "other")) {
        expect_equal(
          .upsampleChannel(channels[[nm]], sf, method),
          ref_upsampleChannel(channels[[nm]], sf, method),
          info = paste(nm, "sf =", sf, "method =", method)
        )
      }
    }
  }
})

test_that("upsample interpolates linearly between samples", {
  w <- tuneR::Wave(c(0, 400, 200), samp.rate=8000, bit=16)

  up <- upsample(w, 32000)               # sf = 4
  expect_equal(up@left, c(0, 100, 200, 300, 400, 350, 300, 250, 200, 200, 200, 200))
})

test_that("upsample holds the final sample rather than producing NAs", {
  w <- tuneR::Wave(c(0, 400), samp.rate=8000, bit=16)

  up <- upsample(w, 24000)               # sf = 3
  expect_false(anyNA(up@left))
  expect_equal(tail(up@left, 3), c(400, 400, 400))
})

test_that("upsample gives the same values on both channels of a stereo wave", {
  set.seed(5)
  l <- round(runif(100, -2^13, 2^13))
  r <- round(runif(100, -2^13, 2^13))
  w <- stereo(
    tuneR::Wave(l, samp.rate=8000, bit=16),
    tuneR::Wave(r, samp.rate=8000, bit=16)
  )

  up <- upsample(w, 8000*3)
  expect_equal(up@left, ref_upsampleChannel(l, 3, "basic"))
  expect_equal(up@right, ref_upsampleChannel(r, 3, "basic"))
})
