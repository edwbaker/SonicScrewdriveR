#A run of pulses to take intervals between. The samples themselves are not read
#by the functions under test, only their count, so a plain ramp will do.
ntdWave <- function(n=1000, samp.rate=1000) {
  tuneR::Wave(round(seq_len(n) %% 100), samp.rate=samp.rate, bit=16)
}

test_that("fewer than two events is an error, not an NA interval", {
  wave <- ntdWave()
  expect_error(ntd(wave, integer(0), function(wave) 1), "at least two events")
  expect_error(ntd(wave, 1, function(wave) 1), "at least two events")
})

test_that("one interval is returned for each pair of consecutive events", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201, 301), function(wave) length(wave@left))
  expect_equal(nrow(res), 3)
  expect_equal(colnames(res), c("k", "Fk"))
})

test_that("the natural time k runs from 1/N to 1", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201, 301), function(wave) length(wave@left))
  expect_equal(res[, "k"], c(1/3, 2/3, 1))
})

test_that("each interval is passed as the wave between one event and the next", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left))
  #The intervals are half open, so an interval of 100 samples gives 100. Both
  #endpoints used to be included and this read c(101, 201).
  expect_equal(res[, "Fk"], c(100, 200))
})

test_that("the interval wave keeps the sample rate and bit depth of the input", {
  wave <- ntdWave(samp.rate=8000)
  seen <- ntd(wave, c(1, 101, 201), function(wave) wave@samp.rate)
  expect_equal(unname(seen[, "Fk"]), c(8000, 8000))
  seen <- ntd(wave, c(1, 101, 201), function(wave) wave@bit)
  expect_equal(unname(seen[, "Fk"]), c(16, 16))
})

test_that("argument='vector' hands the function the samples rather than the wave", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201), function(x) sum(x), argument="vector")
  expected <- c(sum(wave@left[1:100]), sum(wave@left[101:200]))
  expect_equal(unname(res[, "Fk"]), expected)
})

test_that("additional arguments reach the applied function", {
  wave <- ntdWave()
  res <- ntd(wave, c(1, 101, 201), function(wave, add) length(wave@left) + add, add=5)
  expect_equal(unname(res[, "Fk"]), c(105, 105))
})

test_that("normalise=TRUE turns the outputs into a probability density", {
  wave <- ntdWave()
  raw <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left))
  dens <- ntd(wave, c(1, 101, 301), function(wave) length(wave@left), normalise=TRUE)
  expect_equal(sum(dens[, "Fk"]), 1)
  expect_equal(dens[, "Fk"], raw[, "Fk"] / sum(raw[, "Fk"]))
})

test_that("ntd keeps the format of the wave it cuts", {
  # pcm was left to tuneR's default of TRUE whatever the input was, so a float
  # wave reached FUN declaring itself integer PCM.
  w32 <- tuneR::Wave(sin(2*pi*(0:39)/10), samp.rate=8000, bit=32, pcm=FALSE)
  expect_equal(
    unname(ntd(w32, events=c(1, 11, 21, 31), FUN=function(wave) wave@pcm)[, 2]),
    c(0, 0, 0)
  )
  expect_equal(
    unname(ntd(w32, events=c(1, 11, 21, 31), FUN=function(wave) wave@bit)[, 2]),
    c(32, 32, 32)
  )

  # tuneR forbids pcm with 64 bit, so dropping pcm=FALSE made ntd unusable on a
  # 64 bit float recording rather than merely mislabelling it.
  w64 <- tuneR::Wave(sin(2*pi*(0:39)/10), samp.rate=8000, bit=64, pcm=FALSE)
  expect_silent(ntd(w64, events=c(1, 11, 21, 31), FUN=function(wave) length(wave)))

  # An integer PCM wave is still integer PCM.
  w16 <- tuneR::Wave(round(sin(2*pi*(0:39)/10)*1000), samp.rate=8000, bit=16, pcm=TRUE)
  expect_equal(
    unname(ntd(w16, events=c(1, 11, 21, 31), FUN=function(wave) wave@pcm)[, 2]),
    c(1, 1, 1)
  )
})

test_that("the intervals partition the signal they are cut from", {
  #Both endpoints used to be included, so a boundary sample was measured twice:
  #on this wave the interval sums came to 242 where the samples between the
  #first and last event come to 210.
  wave <- tuneR::Wave(as.numeric(1:21), samp.rate=8000, bit=32, pcm=TRUE)
  sums <- ntd(wave, events=c(1, 11, 21), FUN=function(wave) sum(wave@left))[, "Fk"]
  expect_equal(unname(sums), c(sum(1:10), sum(11:20)))
  expect_equal(sum(sums), sum(wave@left[1:20]))

  lengths <- ntd(wave, events=c(1, 11, 21), FUN=function(wave) length(wave))[, "Fk"]
  expect_equal(unname(lengths), diff(c(1, 11, 21)))
})

test_that("the last event bounds the final interval without belonging to it", {
  #The sample at the last event is the onset of the interval that follows the
  #ones measured, so it falls outside every interval cut here.
  wave <- tuneR::Wave(as.numeric(1:21), samp.rate=8000, bit=32, pcm=TRUE)
  seen <- ntd(wave, events=c(1, 11, 21), FUN=function(wave) max(wave@left))[, "Fk"]
  expect_equal(unname(seen), c(10, 20))
})

test_that("an interval is cut as windowing() cuts a window of the same length", {
  #windowing() uses from=start, to=start+window.length-1, and the two agree now.
  wave <- ntdWave()
  events <- c(1, 101, 301)
  seen <- ntd(wave, events, FUN=function(wave) sum(wave@left))[, "Fk"]
  expected <- vapply(seq_len(length(events) - 1), function(i) {
    sum(cutws(wave, from=events[i], to=events[i] + diff(events)[i] - 1)@left)
  }, numeric(1))
  expect_equal(unname(seen), expected)
})

test_that("events that do not increase are an error rather than a reversed interval", {
  #`:` counts downwards when its second value is the smaller, so events out of
  #order used to give the samples between them reversed, and a repeated event a
  #one-sample interval that bounds nothing.
  wave <- ntdWave()
  expect_error(ntd(wave, c(101, 11), function(wave) length(wave)), "increasing order")
  expect_error(ntd(wave, c(1, 11, 11, 21), function(wave) length(wave)), "increasing order")
  expect_error(ntd(wave, c(1, 201, 101), function(wave) length(wave)), "increasing order")
})

test_that("outputs that sum to zero are returned unnormalised with a warning", {
  #Fk/sum(Fk) is NaN throughout where the total is zero, which digital silence
  #and any FUN returning zero for every interval both give, in place of the
  #probability density normalise=TRUE documents.
  wave <- ntdWave()
  expect_warning(dens <- ntd(wave, c(1, 101, 201), function(wave) 0, normalise=TRUE), "sum to zero")
  expect_equal(unname(dens[, "Fk"]), c(0, 0))
  expect_false(any(is.nan(dens[, "Fk"])))

  #A silent recording measured with a FUN that reads its samples reaches the
  #same case.
  silence <- tuneR::Wave(rep(0, 300), samp.rate=1000, bit=16, pcm=TRUE)
  expect_warning(
    dens <- ntd(silence, c(1, 101, 201), function(wave) sum(abs(wave@left)), normalise=TRUE),
    "sum to zero"
  )
  expect_equal(unname(dens[, "Fk"]), c(0, 0))

  #Outputs of opposite sign that cancel gave Inf and -Inf rather than NaN, and
  #have no density either. Nothing is lost by returning them as they are.
  expect_warning(
    out <- ntd(wave, c(1, 101, 201), function(wave) if (length(wave) > 0) 1 else 0, normalise=TRUE),
    NA
  )
  expect_equal(unname(out[, "Fk"]), c(0.5, 0.5))
  signs <- c(1, -1)
  i <- 0
  expect_warning(
    out <- ntd(wave, c(1, 101, 201), function(wave) {i <<- i + 1; signs[i]}, normalise=TRUE),
    "sum to zero"
  )
  expect_equal(unname(out[, "Fk"]), c(1, -1))
  expect_false(any(is.infinite(out[, "Fk"])))
})

test_that("a total that is missing rather than zero is left to divide as before", {
  #An output FUN could not measure is a different matter from a total of
  #nothing, and is not what this guard is for.
  wave <- ntdWave()
  expect_silent(out <- ntd(wave, c(1, 101, 201), function(wave) NA_real_, normalise=TRUE))
  expect_true(all(is.na(out[, "Fk"])))
})
