# Reference implementation: a verbatim copy of the original loop-based
# .ste_dietrich2004(), retained so the vectorised version can be checked against it.
ref_ste <- function(wave, U) {
  e <- vector(mode="numeric", length=length(wave))
  for (i in (U/2+1):(length(wave)-U/2)) {
    values <- (i-U/2):(i+U/2)
    values <- values[values > 0]
    e[i] <- sum(abs(wave@left[values]))
  }
  e
}

test_that("ste matches the reference implementation", {
  set.seed(1)
  w <- tuneR::Wave(round(runif(2000, -2^14, 2^14)), samp.rate=8000, bit=16)

  for (U in c(2, 4, 8, 120)) {
    expect_equal(.ste_dietrich2004(w, U=U), ref_ste(w, U))
  }
  for (U in c(3, 7)) {
    expect_equal(.ste_dietrich2004(w, U=U), ref_ste(w, U))
  }
})

test_that("ste dispatches on method", {
  set.seed(2)
  w <- tuneR::Wave(round(runif(500, -2^14, 2^14)), samp.rate=8000, bit=16)

  expect_equal(ste(w, method="dietrich2004", U=8), .ste_dietrich2004(w, U=8))
  expect_equal(ste(w, U=8), .ste_dietrich2004(w, U=8))

  # An unrecognised method now says so, as it does in every other dispatcher in
  # the package. It used to return NULL.
  expect_error(ste(w, method="not-a-method", U=8), "Unknown method for ste: not-a-method")
})

test_that("ste returns zeroes when the wave is shorter than the window", {
  w <- tuneR::Wave(rep.int(1, 10), samp.rate=8000, bit=16)

  expect_equal(.ste_dietrich2004(w, U=20), vector(mode="numeric", length=10))
})


test_that("ste has a window length of its own", {
  # U had no default, so the documented example ste(sheep, method="dietrich2004")
  # failed with a message naming an argument that is not documented anywhere.
  w <- tuneR::Wave(round(runif(500, -2^14, 2^14)), samp.rate=8000, bit=16)
  expect_equal(eval(formals(ste)$U), 120)
  expect_silent(ste(w))
  expect_equal(ste(w), .ste_dietrich2004(w, U=120))
  expect_false(identical(ste(w, U=8), ste(w, U=120)))
})
