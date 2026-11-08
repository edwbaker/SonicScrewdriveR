# A song that groups: runs of pulses separated by a silence many times the one
# inside a run. groups gives the number of pulses in each run.
echemeFixture <- function(groups, within=50, between=2000, len=50) {
  onsets <- numeric(0)
  at <- 0
  for (g in groups) {
    onsets <- c(onsets, at + (seq_len(g) - 1)*(len + within))
    at <- onsets[length(onsets)] + len + between
  }
  list(onsets=onsets, offsets=onsets + len)
}

test_that("echemes groups the pulses of a song that groups", {
  p <- echemeFixture(rep(4, 10))
  r <- echemes(p)
  expect_equal(length(r$onsets), 10)
  expect_equal(r$pulses, rep(4L, 10))
  #An echeme follows the pulse boundary convention of R/pulseDetection.R: its
  #onset is the first sample of its first pulse and its offset the first sample
  #after its last pulse.
  expect_equal(r$onsets[1], p$onsets[1])
  expect_equal(r$offsets[1], p$offsets[4])
  expect_equal(r$onsets[10], p$onsets[37])
  expect_equal(r$offsets[10], p$offsets[40])
  #Separated this cleanly the division accounts for all of the variance.
  expect_equal(r$separation, 1)
  #The silence between echemes is the stated multiple of the one within them.
  expect_equal(r$ratio, 2000/50)
})

test_that("echemes reports none for a continuous trill", {
  # The failure this function exists to prevent. A trill has one kind of
  # silence, and splitting it at the longest gap reported echemes that are not
  # in the song: on 27 recordings of Oecanthus pictus that gave between 1 and
  # 468 pulses per echeme for the one species.
  r <- echemes(list(onsets=seq(0, by=100, length.out=40),
                    offsets=seq(0, by=100, length.out=40) + 50))
  expect_equal(length(r$onsets), 0)
  expect_equal(length(r$offsets), 0)
  expect_equal(length(r$pulses), 0)
  #Every silence the same leaves no variance for a division to account for.
  expect_equal(r$separation, 0)
})

test_that("echemes reports none for a trill that is merely uneven", {
  # Jitter gives a division something to find, but not two groups: the share of
  # the variance it accounts for stays below the default.
  set.seed(1)
  onsets <- cumsum(c(0, round(stats::rnorm(39, 100, 8))))
  r <- echemes(list(onsets=onsets, offsets=onsets + 20))
  expect_equal(length(r$onsets), 0)
  expect_lt(r$separation, 0.8)
  #Both conditions refuse it: the division accounts for too little of the
  #variance, and what it divides into is mostly single pulses. Dropping both
  #bars reports the division that was found, so nothing else is in the way.
  expect_equal(length(echemes(list(onsets=onsets, offsets=onsets + 20),
                              min.separation=0)$onsets), 0)
  expect_gt(length(echemes(list(onsets=onsets, offsets=onsets + 20),
                           min.separation=0, min.pulses=1)$onsets), 0)
})

test_that("echemes refuses a division that puts each pulse in its own group", {
  # A few short silences among many long ones divide cleanly, so separation
  # alone accepts it, but almost every group holds one pulse. A group of one
  # pulse is a pulse, not a group of pulses.
  gaps <- c(rep(60, 4), rep(1200, 25))
  onsets <- cumsum(c(0, gaps + 50))
  p <- list(onsets=onsets, offsets=onsets + 50)
  r <- echemes(p)
  expect_equal(length(r$onsets), 0)
  #The division itself was a good one, which is why min.pulses is needed as
  #well as min.separation.
  expect_gt(r$separation, 0.8)
  expect_gt(length(echemes(p, min.pulses=1)$onsets), 0)
})

test_that("echemes counts the pulses of each echeme in order", {
  # table() would order the group numbers as text, putting echeme 10 before
  # echeme 2, so the counts would no longer line up with the boundaries beside
  # them. The groups differ in size, so a misordering shows.
  groups <- c(3, 6, 3, 3, 4, 3, 5, 3, 3, 3, 7, 3)
  p <- echemeFixture(groups)
  r <- echemes(p)
  expect_equal(length(r$onsets), length(groups))
  expect_equal(r$pulses, as.integer(groups))
  #Each reported echeme holds as many pulses as its own boundaries span.
  spanned <- vapply(seq_along(r$onsets), function(i)
    sum(p$onsets >= r$onsets[i] & p$offsets <= r$offsets[i]), numeric(1))
  expect_equal(spanned, as.numeric(groups))
})

test_that("echemes groups pulses that touch", {
  # A distance of no samples has no logarithm. Leaving those distances out of
  # the division lost the whole short group from a song whose pulses all touch,
  # and no division could be found at all.
  p <- echemeFixture(rep(4, 10), within=0)
  expect_equal(min(p$onsets[-1] - p$offsets[-length(p$offsets)]), 0)
  r <- echemes(p)
  expect_equal(length(r$onsets), 10)
  expect_equal(r$pulses, rep(4L, 10))
})

test_that("echemes needs enough silences to divide", {
  # Two on each side of a division is four silences, which is five pulses.
  for (n in 2:4) {
    onsets <- seq(0, by=1000, length.out=n)
    r <- echemes(list(onsets=onsets, offsets=onsets + 50))
    expect_equal(length(r$onsets), 0)
    expect_true(is.na(r$separation))
  }
  expect_equal(length(echemes(echemeFixture(c(2, 2, 2)))$onsets), 3)
})

test_that("echemes leaves out a pulse with no offset", {
  # A recording can end part way through a pulse, leaving one more onset than
  # offset. Its length is unknown, so it cannot close an echeme.
  p <- echemeFixture(rep(4, 10))
  p$onsets <- c(p$onsets, 100000)
  r <- echemes(p)
  expect_equal(length(r$onsets), 10)
  expect_equal(r$pulses, rep(4L, 10))
  expect_equal(max(r$offsets), max(p$offsets))
})

test_that("echemes refuses input it cannot use", {
  p <- echemeFixture(rep(4, 10))
  expect_error(echemes(list(onsets=p$onsets)), "must hold offsets")
  expect_error(echemes(p, min.separation=2), "between 0 and 1")
  expect_error(echemes(p, min.separation=-1), "between 0 and 1")
  expect_error(echemes(p, min.separation="half"), "between 0 and 1")
  expect_error(echemes(p, min.pulses=0), "positive whole number")
  expect_error(echemes(p, min.pulses=2.5), "positive whole number")
  #pulseStats() is what refuses onsets and offsets describing different pulses.
  expect_error(
    echemes(list(onsets=c(0, 100, 200), offsets=c(50, 150, 120))),
    "do not describe the same pulses"
  )
})

test_that("echemes works on what a detector actually returns", {
  # The fixtures above are written by hand; this checks the result of a real
  # detector flows through unchanged.
  x <- numeric(44100)
  p <- echemeFixture(rep(3, 8), within=300, between=4000, len=200)
  for (i in seq_along(p$onsets)) {
    x[(p$onsets[i] + 1):p$offsets[i]] <- 1
  }
  w <- tuneR::Wave(x, samp.rate=44100, bit=32, pcm=FALSE)
  r <- echemes(pulseDetection(w, method="threshold"))
  expect_equal(length(r$onsets), 8)
  expect_equal(r$pulses, rep(3L, 8))
})
