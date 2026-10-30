test_that("PseudoWave creation", {
  expect_error(inherits(pseudoWave(), "Either type or url must be specified"))
  expect_true(inherits(pseudoWave("noise", "white"), "PseudoWave"))
  expect_equal(pseudoWave("noise", "white")@type, "noise")
  expect_equal(pseudoWave("noise", "white")@subtype, "white")
  expect_equal(pseudoWave("noise", "white", scale=2)@scale, 2)
  expect_equal(pseudoWave("noise", "white", offset=3)@offset, 3)
  expect_equal(pseudoWave("noise", "white", seed=5)@seed, 5)
})

test_that("depseduoWave tests", {
  # Generate mono and stero sine waves
  m <- tuneR::sine(440, duration=44100, samp.rate=44100)
  s <- tuneR::sine(440, duration=44100, samp.rate=44100, stereo=TRUE)

  # Generate a white noise PseudoWave
  pw <- pseudoWave("noise", "white")

  # Add PseudoWave to sine
  mpw <- m + pw
  spw <- s + pw

  rmpw <- pw + m
  rspw <- pw + s

  # Should return a Wave object
  expect_true(inherits(mpw, "Wave"))
  expect_true(inherits(spw, "Wave"))
  expect_true(inherits(rmpw, "Wave"))
  expect_true(inherits(rspw, "Wave"))
  expect_equal(length(mpw@left), 44100)
  expect_equal(length(spw@left), 44100)
  expect_equal(length(rmpw@left), 44100)
  expect_equal(length(spw@right), 44100)
  expect_equal(length(rspw@right), 44100)

  # Generate mono and stereo silence
  mq <- tuneR::silence(44100, samp.rate=44100)
  sq <- tuneR::silence(44100, samp.rate=44100, stereo=TRUE)

  # Multiply PseudoWave by silence
  mqw <- mq * pw
  sqw <- sq * pw

  # Should return a silent Wave object
  expect_true(inherits(mqw, "Wave"))
  expect_true(inherits(sqw, "Wave"))
  expect_equal(sum(mqw@left), 0)
  expect_equal(sum(sqw@right), 0)

  # Generate sum of silence and sine PseudoWave
  spw1 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440))
  expect_lte(max(spw1@left), 1)
  expect_lte(mean(spw1@left), 0.001)

  # Test scale parameter and multiplication / division
  spw2 <- tuneR::silence(duration=44100) + pseudoWave("sine", scale=0.5, params=list(f0=440))
  expect_lte(max(spw2@left), 0.5)
  expect_lte(mean(spw2@left), 0.001)

  spw4 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440)) * 0.5
  expect_lte(max(spw4@left), 0.5)
  expect_lte(mean(spw4@left), 0.001)

  spw6 <- tuneR::silence(duration=44100) + 0.5 * pseudoWave("sine", params=list(f0=440))
  expect_lte(max(spw4@left), 0.5)
  expect_lte(mean(spw4@left), 0.001)

  spw8 <- tuneR::silence(duration=44100) + pseudoWave("sine", params=list(f0=440)) / 2
  expect_lte(max(spw8@left), 0.5)
  expect_lte(mean(spw8@left), 0.001)

  # Test offset parameter and addition / subtraction
  spw3 <- tuneR::silence(duration=44100) + pseudoWave("sine", offset=1, params=list(f0=440))
  expect_lte(max(spw3@left), 2)
  expect_gte(min(spw3@left), 0)
  expect_lte(mean(spw3@left), 1.001)
  expect_gte(mean(spw3@left), 0.999)

  spw5 <- tuneR::silence(duration=44100) + (pseudoWave("sine", params=list(f0=440)) + 1)
  expect_lte(max(spw5@left), 2)
  expect_gte(min(spw5@left), 0)
  expect_lte(mean(spw5@left), 1.001)
  expect_gte(mean(spw5@left), 0.999)

  spw7 <- tuneR::silence(duration=44100) + (1 + pseudoWave("sine", params=list(f0=440)))
  expect_lte(max(spw7@left), 2)
  expect_gte(min(spw7@left), 0)
  expect_lte(mean(spw7@left), 1.001)
  expect_gte(mean(spw7@left), 0.999)

  spw9 <- tuneR::silence(duration=44100) + (pseudoWave("sine", params=list(f0=440)) - 0.50)
  expect_lte(max(spw9@left), 0.5)
  expect_lte(mean(spw9@left), 0.001)
})

test_that("pseudoWave is reproducible by default", {
  # The default seed is what makes the same PseudoWave give the same samples each
  # time. The class prototype used to disagree with the constructor about it.
  expect_equal(eval(formals(pseudoWave)$seed), 1)
  expect_equal(pseudoWave("noise", "white")@seed, new("PseudoWave")@seed)

  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_equal(
    (m + pseudoWave("noise", "white"))@left,
    (m + pseudoWave("noise", "white"))@left
  )
})

test_that("a seed of NA asks for a fresh sequence each time", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  # A bare NA is logical, and the slot holds a number, so this used to be
  # rejected by the class.
  for (none in list(NA, NA_integer_, NA_real_)) {
    expect_false(identical(
      (m + pseudoWave("noise", "white", seed=none))@left,
      (m + pseudoWave("noise", "white", seed=none))@left
    ))
  }
})

test_that("pseudoWave takes one seed", {
  expect_error(pseudoWave("noise", "white", seed=c(1, 2)), "a single seed")
})

test_that("different seeds give different noise", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_false(identical(
    (m + pseudoWave("noise", "white", seed=5))@left,
    (m + pseudoWave("noise", "white", seed=6))@left
  ))
})

test_that("an explicit seed still gives reproducible noise", {
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)
  expect_equal(
    (m + pseudoWave("noise", "white", seed=5))@left,
    (m + pseudoWave("noise", "white", seed=5))@left
  )
})

test_that("a seeded PseudoWave leaves the caller's random numbers alone", {
  # set.seed() replaces the random stream of the whole session, so every random
  # number drawn after a seeded PseudoWave used to change.
  m <- tuneR::sine(440, duration=1000, samp.rate=44100)

  set.seed(99)
  before <- runif(3)

  set.seed(99)
  invisible(m + pseudoWave("noise", "white", seed=5))
  after <- runif(3)

  expect_equal(before, after)
})

test_that("a file PseudoWave gives as many samples as the wave it meets", {
  # The file was read whole and then recycled against the target by the
  # arithmetic, whatever the two lengths were.
  f <- tempfile(fileext=".wav")
  tuneR::writeWave(tuneR::sine(300, duration=500, samp.rate=44100, bit=16, pcm=TRUE), f)
  pw <- pseudoWave("file", f)

  for (n in c(200, 500, 1300)) {
    target <- tuneR::sine(440, duration=n, samp.rate=44100, bit=16, pcm=TRUE)
    expect_equal(length((target + pw)@left), n)
  }
})

test_that("pseudoWave rejects a type it cannot generate", {
  # An unknown type left the wave unassigned, so the arithmetic failed with
  # "object 'w' not found".
  expect_error(pseudoWave("dog"), "Unsupported PseudoWave type: dog")
  expect_error(
    depseduoWave(new("PseudoWave", type="dog"), 10, FALSE, 44100, 16, TRUE),
    "Unsupported PseudoWave type: dog"
  )
})

test_that("a file PseudoWave needs a file to be named", {
  expect_error(pseudoWave("file"), "Filename must be specified")
  expect_error(pseudoWave("file", NA_character_), "Filename must be specified")
})

test_that("a file PseudoWave names a file that is there", {
  missing <- file.path(tempdir(), "no-such-pseudowave.wav")
  expect_error(pseudoWave("file", missing), "File does not exist")
})

test_that("a stereo file PseudoWave fills both channels to the length wanted", {
  f <- tempfile(fileext=".wav")
  withr::defer(unlink(f))
  left <- tuneR::sine(300, duration=500, samp.rate=44100, bit=16, pcm=TRUE)
  stereo <- tuneR::Wave(
    left = left@left,
    right = rev(left@left),
    samp.rate = 44100, bit = 16, pcm = TRUE
  )
  tuneR::writeWave(stereo, f)
  pw <- pseudoWave("file", f)

  for (n in c(200, 500, 1300)) {
    target <- tuneR::Wave(
      left = rep(0, n), right = rep(0, n), samp.rate = 44100, bit = 16, pcm = TRUE
    )
    result <- target + pw
    expect_true(result@stereo)
    expect_equal(length(result@left), n)
    expect_equal(length(result@right), n)
  }
})

test_that("a seeded PseudoWave leaves a session that had no random stream without one", {
  #The stream is put back as it was, which where there was none means removing
  #the one that setting the seed created.
  if (exists(".Random.seed", envir=globalenv(), inherits=FALSE)) {
    saved <- get(".Random.seed", envir=globalenv(), inherits=FALSE)
    withr::defer(assign(".Random.seed", saved, envir=globalenv()))
    rm(".Random.seed", envir=globalenv())
  }
  target <- tuneR::Wave(rep(0, 100), samp.rate=44100, bit=16, pcm=TRUE)
  invisible(target + pseudoWave("noise", "white", seed=5))
  expect_false(exists(".Random.seed", envir=globalenv(), inherits=FALSE))
})

test_that("a seeded PseudoWave still gives the same samples with no stream to start from", {
  dropStream <- function() {
    if (exists(".Random.seed", envir=globalenv(), inherits=FALSE)) {
      rm(".Random.seed", envir=globalenv())
    }
  }
  if (exists(".Random.seed", envir=globalenv(), inherits=FALSE)) {
    saved <- get(".Random.seed", envir=globalenv(), inherits=FALSE)
    withr::defer(assign(".Random.seed", saved, envir=globalenv()))
  }
  target <- tuneR::Wave(rep(0, 100), samp.rate=44100, bit=16, pcm=TRUE)
  pw <- pseudoWave("noise", "white", seed=5)

  dropStream()
  first <- (target + pw)@left
  dropStream()
  second <- (target + pw)@left
  expect_equal(first, second)
})

test_that("a PseudoWave takes a single seed", {
  expect_error(pseudoWave("noise", "white", seed=c(1, 2)), "single seed")
  expect_error(pseudoWave("noise", "white", seed=numeric(0)), "single seed")
})

test_that("a number minus a PseudoWave inverts the wave about that number", {
  # 1 - noise is the obvious way to write an inverted source, and had no method.
  base <- tuneR::silence(duration=1000)
  pw <- pseudoWave("sine", params=list(f0=440))

  up <- base + pw
  down <- base + (1 - pw)

  # Inverting about 1 means every sample becomes 1 minus itself.
  expect_equal(down@left, 1 - up@left)
  expect_equal(max(down@left), 1 - min(up@left))
  expect_equal(min(down@left), 1 - max(up@left))
})

test_that("a number minus a PseudoWave accounts for a scale and an offset already set", {
  # The subtraction has to compose with whatever the PseudoWave already holds,
  # not just with a fresh one: 5 - ((g * 2) + 3) is (g * -2) + 2.
  base <- tuneR::silence(duration=1000)
  pw <- pseudoWave("sine", scale=2, offset=3, params=list(f0=440))

  plain <- (base + pw)@left
  flipped <- (base + (5 - pw))@left

  expect_equal(flipped, 5 - plain)
})

test_that("dividing a number by a PseudoWave is refused with a reason", {
  pw <- pseudoWave("sine", params=list(f0=440))
  expect_error(5 / pw, "Cannot divide a number by a PseudoWave")
})

test_that("a scale applied after an offset takes the offset into it", {
  # depseudoWave() always applies scale then offset, but the operators used to
  # accumulate into the two slots independently, so an offset written BEFORE a
  # scale was applied AFTER it and the order the caller wrote was discarded.
  base <- tuneR::silence(duration=1000)
  pw <- pseudoWave("sine", params=list(f0=1))

  # The sine starts at zero, so sample 1 shows the offset alone.
  expect_equal((base + (pw + 5) * 2)@left[1], 10)
  expect_equal((base + 2 * (pw + 5))@left[1], 10)
  expect_equal((base + (pw - 1) * 3)@left[1], -3)
  expect_equal((base + (pw + 1) / 2)@left[1], 0.5)

  # A scale written first was always right, and still is.
  expect_equal((base + (pw * 2) + 5)@left[1], 5)
})

test_that("lazy PseudoWave arithmetic agrees with eager arithmetic", {
  # The class documents itself as "converted to a Wave object when operated
  # on", so operating lazily must give what operating on the Wave would.
  base <- tuneR::silence(duration=1000)
  pw <- pseudoWave("sine", params=list(f0=1))

  plain <- (base + pw)@left

  expect_equal((base + (pw + 5) * 2)@left, (plain + 5) * 2)
  expect_equal((base + 2 * (pw + 5))@left, 2 * (plain + 5))
  expect_equal((base + (pw - 1) * 3)@left, (plain - 1) * 3)
  expect_equal((base + (pw + 1) / 2)@left, (plain + 1) / 2)
  expect_equal((base + (pw * 2) + 5)@left, (plain * 2) + 5)

  # Composing three operators in a row, which no single slot can reorder.
  expect_equal((base + ((pw + 1) * 2 - 3) * 4)@left, ((plain + 1) * 2 - 3) * 4)
})

test_that("unipolar noise at half scale is bounded as written", {
  # The case named in the report: (noise + 1) * 0.5 is the obvious way to write
  # unipolar noise at half scale, and instead gave bipolar noise at half scale
  # with a full-size DC offset.
  base <- tuneR::silence(duration=2000)
  pw <- (pseudoWave("noise", "white", seed=42) + 1) * 0.5

  s <- (base + pw)@left
  expect_gte(min(s), 0)
  expect_lte(max(s), 1)
})
