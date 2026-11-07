# A wave can declare a format its samples do not fit. tuneR::Wave() declares
# 16-bit PCM unless told otherwise, so floating point samples given to it make
# a wave claiming to hold whole numbers, and every function that rebuilds a wave
# rounds them: cutws() of tuneR::Wave(runif(n, -1, 1)) returned -1, 0 and 1.

floatsAsPcm <- function(n=800, bit=16) {
  tuneR::Wave(stats::runif(n, -1, 1), samp.rate=8000, bit=bit)
}

#The number of format warnings an expression raises, which are muffled.
formatWarnings <- function(expr) {
  n <- 0
  withCallingHandlers(
    expr,
    waveFormatMismatch = function(w) {
      n <<- n + 1
      invokeRestart("muffleWarning")
    }
  )
  return(n)
}

test_that("a PCM wave holding samples that are not whole is warned about", {
  set.seed(291)
  bad <- floatsAsPcm()
  expect_warning(
    validateIsWave(bad),
    "declares 16-bit PCM but holds samples that are not whole numbers",
    class = "waveFormatMismatch"
  )
  expect_warning(validateIsWaveLike(bad), class = "waveFormatMismatch")
  mc <- tuneR::WaveMC(matrix(stats::runif(200, -1, 1), ncol=2), samp.rate=8000, bit=24)
  expect_warning(validateIsWaveMC(mc), "declares 24-bit PCM", class = "waveFormatMismatch")
  expect_warning(validateIsWaveLike(mc), class = "waveFormatMismatch")
  #The right channel is read as well as the left.
  right <- tuneR::Wave(rep(0, 100), stats::runif(100, -1, 1), samp.rate=8000, bit=16)
  expect_warning(validateIsWave(right), class = "waveFormatMismatch")
  #A tagged wave is a wave like any other.
  expect_warning(tagWave(bad), class = "waveFormatMismatch")
})

test_that("the format warning leaves the wave as it was and names no call", {
  set.seed(291)
  bad <- floatsAsPcm()
  expect_identical(suppressWarnings(validateIsWave(bad)), bad)
  cond <- tryCatch(validateIsWave(bad), warning=function(w) w)
  expect_s3_class(cond, c("waveFormatMismatch", "warning", "condition"), exact=TRUE)
  expect_null(conditionCall(cond))
  #A warning, so that suppressWarnings() and options(warn) treat it as one.
  expect_silent(suppressWarnings(validateIsWave(bad)))
})

test_that("waves whose samples fit their format are not warned about", {
  set.seed(291)
  expect_silent(validateIsWave(tuneR::Wave(-3:3, samp.rate=8000, bit=16)))
  expect_silent(validateIsWave(tuneR::Wave(c(-32768, 0, 32767), samp.rate=8000, bit=16)))
  expect_silent(validateIsWave(tuneR::Wave(c(0, 128, 255), samp.rate=8000, bit=8)))
  expect_silent(validateIsWave(tuneR::Wave(c(-2^23, 2^23 - 1), samp.rate=8000, bit=24)))
  expect_silent(validateIsWave(tuneR::Wave(c(-2^31, 2^31 - 1), samp.rate=8000, bit=32)))
  expect_silent(validateIsWave(tuneR::sine(1000, bit=16, pcm=TRUE)))
  expect_silent(validateIsWave(tuneR::sine(1000)))
  expect_silent(validateIsWave(
    tuneR::Wave(stats::runif(100, -1, 1), samp.rate=8000, bit=64, pcm=FALSE)
  ))
  expect_silent(validateIsWaveMC(
    tuneR::WaveMC(matrix(-5:4, ncol=2), samp.rate=8000, bit=16)
  ))
  #Nothing to check.
  expect_silent(validateIsWave(tuneR::Wave(numeric(0), samp.rate=8000, bit=16)))
  unknown <- tuneR::Wave(c(0, 0.5, 2), samp.rate=8000, bit=32, pcm=FALSE)
  unknown@pcm <- NA
  expect_true(validObject(unknown))
  expect_silent(validateIsWave(unknown))
  expect_silent(validateIsWave(tuneR::Wave(c(NA, 1, NA), samp.rate=8000, bit=16)))
  expect_silent(validateIsWave(tuneR::Wave(c(NA_real_, NA_real_), samp.rate=8000, bit=16)))
})

test_that("PCM samples outside the range of the format are warned about", {
  outside <- function(samples, bit) {
    validateIsWave(tuneR::Wave(samples, samp.rate=8000, bit=bit))
  }
  #Eight bit PCM is unsigned about 128, so a wave centred on zero is outside it.
  expect_warning(
    outside(c(-100, 0, 100), 8),
    "declares 8-bit PCM but holds samples outside the range 0 to 255",
    class = "waveFormatMismatch"
  )
  expect_warning(outside(c(0, 256), 8), "0 to 255", class = "waveFormatMismatch")
  expect_warning(outside(c(0, 32768), 16), "-32768 to 32767", class = "waveFormatMismatch")
  expect_warning(outside(c(-32769, 0), 16), "-32768 to 32767", class = "waveFormatMismatch")
  expect_warning(outside(c(0, 2^23), 24), "-8388608 to 8388607", class = "waveFormatMismatch")
  expect_warning(
    outside(c(-2^31 - 1, 0), 32), "-2147483648 to 2147483647", class = "waveFormatMismatch"
  )
  expect_warning(outside(c(0, Inf), 16), "outside the range", class = "waveFormatMismatch")
  #A wave can be at fault both ways, and is warned about each.
  expect_warning(
    expect_warning(outside(c(0.5, 40000), 16), "not whole numbers"),
    "outside the range"
  )
})

test_that("floating point samples outside -1 to 1 are warned about", {
  float <- function(samples, bit=32) {
    validateIsWave(tuneR::Wave(samples, samp.rate=8000, bit=bit, pcm=FALSE))
  }
  expect_warning(
    float(c(0, 1.5)),
    "declares floating point samples but holds samples outside the range -1 to 1",
    class = "waveFormatMismatch"
  )
  expect_warning(float(c(-1.01, 0), bit=64), "-1 to 1", class = "waveFormatMismatch")
  expect_warning(float(c(0, -Inf)), "-1 to 1", class = "waveFormatMismatch")
  expect_silent(float(c(-1, 0, 1)))
  expect_silent(float(c(-0.25, 0.5)))
})

test_that("a PCM depth with no range of its own is checked only for whole samples", {
  #tuneR's validity rules accept a 1-bit PCM wave, which it does not write.
  expect_silent(validateIsWave(tuneR::Wave(c(0, 1, 2), samp.rate=8000, bit=1, pcm=TRUE)))
  expect_warning(
    validateIsWave(tuneR::Wave(c(0, 0.5), samp.rate=8000, bit=1, pcm=TRUE)),
    "declares 1-bit PCM", class = "waveFormatMismatch"
  )
})

test_that("the check reads the same 1024 samples spread over each channel", {
  long <- tuneR::Wave(rep(0, 1e5), samp.rate=8000, bit=16)
  #The first and last samples are read, and most of those between are not, so a
  #fault confined to a few samples can be missed.
  first <- long
  first@left[1] <- 0.5
  expect_warning(validateIsWave(first), class = "waveFormatMismatch")
  last <- long
  last@left[1e5] <- 0.5
  expect_warning(validateIsWave(last), class = "waveFormatMismatch")
  missed <- long
  missed@left[2] <- 0.5
  expect_silent(validateIsWave(missed))
  expect_equal(length(.formatSamples(long)), 1024)
  expect_equal(.formatSamples(long), .formatSamples(long))
  #A wave of 1024 samples or fewer is read whole.
  short <- tuneR::Wave(rep(0, 1024), samp.rate=8000, bit=16)
  short@left[2] <- 0.5
  expect_warning(validateIsWave(short), class = "waveFormatMismatch")
  expect_equal(length(.formatSamples(short)), 1024)
  #Every channel is read at the same places.
  mc <- tuneR::WaveMC(matrix(0, nrow=1e5, ncol=3), samp.rate=8000, bit=16)
  expect_equal(length(.formatSamples(mc)), 3 * 1024)
  mc@.Data[1e5, 3] <- 0.5
  expect_warning(validateIsWaveMC(mc), class = "waveFormatMismatch")
})

test_that("each function that rebuilds a wave warns before rounding one", {
  set.seed(291)
  bad <- floatsAsPcm(n=8000)
  stereo <- tuneR::Wave(
    stats::runif(8000, -1, 1), stats::runif(8000, -1, 1), samp.rate=8000, bit=16
  )
  mc <- tuneR::WaveMC(matrix(stats::runif(8000 * 3, -1, 1), ncol=3), samp.rate=8000, bit=16)
  #These cut, filter or take a channel of the wave, and so round its samples.
  expect_warning(cutws(bad, 1, 100), class = "waveFormatMismatch")
  expect_warning(upsample(bad, 16000), class = "waveFormatMismatch")
  expect_warning(ntd(bad, c(1, 101, 201), function(wave) length(wave)), class = "waveFormatMismatch")
  expect_warning(filterWave(bad, bandpass(500, 2000)), class = "waveFormatMismatch")
  expect_warning(autoBandPass(bad, lowcut=100), class = "waveFormatMismatch")
  expect_warning(generateTimeShift(bad, amount=0.01), class = "waveFormatMismatch")
  expect_warning(allChannels(mc, length, channel.param=NULL), class = "waveFormatMismatch")
  expect_warning(fcis(mc, window.length=512, wl=256), class = "waveFormatMismatch")
  expect_warning(fcis(mc, window.length=512, wl=256, channel=2), class = "waveFormatMismatch")
  expect_warning(fcis(stereo, window.length=512, wl=256, channel=1), class = "waveFormatMismatch")
})

test_that("a function working through one wave many times warns about it once", {
  set.seed(291)
  bad <- floatsAsPcm(n=8000)
  stereo <- tuneR::Wave(
    stats::runif(8000, -1, 1), stats::runif(8000, -1, 1), samp.rate=8000, bit=16
  )
  #80 windows, each cut from the whole wave by cutws().
  expect_equal(formatWarnings(windowing(bad, window.length=100, FUN=function(w, ...) 1)), 1)
  #A gap between the windows is cut from the wave too, when they are bound.
  expect_equal(formatWarnings(windowing(
    bad, window.length=100, window.overlap=-50, bind.wave=TRUE, FUN=function(w, ...) w
  )), 1)
  expect_equal(formatWarnings(generateTimeShift(bad, amount=c(0.01, 0.02), where="both")), 1)
  expect_equal(formatWarnings(generateTimeShift(bad, type="rotate", amount=c(0.01, 0.02))), 1)
  #FUN is given the same channels, which a validator in it would check again.
  expect_equal(formatWarnings(allChannels(stereo, validateIsWave, channel.param=NULL)), 1)
  expect_equal(formatWarnings(fcis(stereo, window.length=512, wl=256)), 1)
  expect_equal(formatWarnings(filterWave(bad, bandpass(500, 2000))), 1)
  expect_equal(formatWarnings(autoBandPass(stereo, lowcut=100)), 1)
  expect_equal(formatWarnings(filterWave(list(bad, bad), bandpass(500, 2000))), 2)
})

test_that("a function handing its wave to another that checks it warns once", {
  set.seed(291)
  bad <- floatsAsPcm(n=8000)
  #zerocross() finds the periods of jitter() and shimmer().
  expect_equal(formatWarnings(jitter(bad)), 1)
  expect_equal(formatWarnings(jitter(bad, method="relative")), 1)
  expect_equal(formatWarnings(shimmer(bad)), 1)
  #ste() is taken over two windows.
  expect_equal(formatWarnings(pulseDetection(bad)), 1)
  expect_equal(formatWarnings(pd_simple(bad)), 1)
  expect_equal(formatWarnings(pd_dietrich2004(bad, filter=FALSE)), 1)
  #A region of the wave is cut with cutws().
  loud <- tuneR::WaveMC(
    matrix(stats::rnorm(4800 * 2) * 10, ncol=2), samp.rate=48000, bit=32, pcm=FALSE
  )
  expect_equal(formatWarnings(tdoa(loud, from=0.01, to=0.05)), 1)
})

test_that("a wave a function gives back is warned about where it is used", {
  set.seed(291)
  tw <- tagWave(tuneR::Wave(round(5000 * stats::rnorm(8000)), samp.rate=8000, bit=16))
  #ffilter() gives 16-bit PCM holding samples that are not whole, and tagApply()
  #tags it without warning, as nothing has been done with it yet.
  expect_equal(
    formatWarnings(tagApply(tw, seewave::ffilter, from=500, to=2000, output="Wave")), 0
  )
  filtered <- tagApply(tw, seewave::ffilter, from=500, to=2000, output="Wave")
  expect_warning(cutws(filtered, 1, 100), class = "waveFormatMismatch")
  #A wave handed to tagWave() is checked like any other.
  expect_warning(tagWave(untagWave(filtered)), class = "waveFormatMismatch")
})

test_that("generateNoise() gives a PCM wave whole samples", {
  #The noise was mixed into the samples unrounded, so the wave it gave back
  #declared a format its samples did not fit.
  set.seed(291)
  for (bit in c(8, 16, 24)) {
    centre <- if (bit == 8) 128 else 0
    scale <- if (bit == 8) 25 else 2^(bit - 4)
    w <- tuneR::Wave(round(scale * stats::rnorm(4000)) + centre, samp.rate=8000, bit=bit)
    noisy <- generateNoise(w, "white", noise.ratio=0.3)
    expect_equal(noisy@left, round(noisy@left), info=bit)
    expect_equal(formatWarnings(cutws(noisy, 1, 100)), 0, info=bit)
  }
  st <- tuneR::stereo(w, w)
  noisy <- generateNoise(st, "white", noise.ratio=0.3)
  expect_equal(noisy@right, round(noisy@right))
  mc <- tuneR::WaveMC(cbind(w@left, w@left), samp.rate=8000, bit=24)
  noisy <- generateNoise(mc, "white", noise.ratio=0.3)
  expect_equal(noisy@.Data, round(noisy@.Data))
  #Floating point samples are left as they are.
  fl <- tuneR::Wave(stats::runif(4000, -0.5, 0.5), samp.rate=8000, bit=32, pcm=FALSE)
  noisy <- generateNoise(fl, "white", noise.ratio=0.3)
  expect_false(isTRUE(all.equal(noisy@left, round(noisy@left))))
  expect_silent(validateIsWave(noisy))
})

test_that("a wave the package makes and only measures is not warned about", {
  #autoBandPass() measures a high-passed copy of the wave, which seewave returns
  #as 16-bit PCM holding samples that are not whole.
  set.seed(291)
  good <- tuneR::Wave(round(5000 * stats::rnorm(8000)), samp.rate=8000, bit=16)
  expect_silent(validateIsWave(good))
  expect_equal(formatWarnings(autoBandPass(good, lowcut=100)), 0)
})

test_that("only the format warning is held back while a wave is worked through", {
  expect_warning(.withoutFormatWarning(warning("another warning")), "another warning")
  expect_equal(.withoutFormatWarning(1 + 1), 2)
})

test_that("normalising a wave into its format is not warned about", {
  set.seed(291)
  bad <- floatsAsPcm()
  expect_silent(normalise(bad))
  expect_silent(validateIsWave(normalise(bad)))
  #data2Wave() makes a 16-bit wave of the data it is given, and normalises it.
  expect_silent(data2Wave(stats::runif(100, -1, 1)))
  expect_silent(validateIsWave(data2Wave(stats::runif(100, -1, 1))))
})
