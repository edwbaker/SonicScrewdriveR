# Reference implementation: the original loop-based pd_dietrich2004(), with the one
# change that the second loop writes to offsets rather than onsets. Retained so the
# vectorised version can be checked against it.
ref_dietrich <- function(
  wave,
  U=120,
  gamma=0.05,
  alpha=1.4,
  scaling=32,
  V=480,
  psi=1
){
  e_u <- ste(wave, U=U)
  e_v <- ste(wave, U=V)
  theta <- min(e_u) + gamma*(max(e_u) - min(e_u))
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  onsets <- vector(mode="logical", length=length(wave))
  tau <- 0
  A <- 0
  for (t in (U/2+1):(length(wave)-U/2)) {
    if (e_u[t] >= F_1[t]) {
      if (tau > 0) {
        if (A==1) {
          if (e_u[t] >= F_a[t]) {
            onsets[t] <- TRUE
            A <- 0
          } else {
            if (tau > psi) {
              tau <- 0
            } else {
              tau <- tau+1
            }
          }
        }
      } else {
        tau <- tau + 1
      }
    } else {
      tau <- 0
      A <- 1
    }
  }
  offsets <- vector(mode="logical", length=length(wave))
  tau <- 0
  A <- 0
  for (t in (U/2+1):(length(wave)-U/2)) {
    if (e_u[t] <= F_1[t]) {
      if (tau > 0) {
        if (A==1) {
          if (e_u[t] <= F_a[t]) {
            offsets[t] <- TRUE
            A <- 0
          } else {
            if (tau > psi) {
              tau <- 0
            } else {
              tau <- tau+1
            }
          }
        }
      } else {
        tau <- tau + 1
      }
    } else {
      tau <- 0
      A <- 1
    }
  }
  return(list(
    onsets = which(onsets == TRUE),
    offsets = which(offsets == TRUE)
  ))
}

# A wave with obvious pulses, and one without, to exercise both branches.
pulsed_wave <- function(n_pulses=6, pulse.length=200, gap=300, samp.rate=8000) {
  set.seed(42)
  x <- unlist(lapply(seq_len(n_pulses), function(i) {
    c(
      sin(seq(0, 20*pi, length.out=pulse.length)) * 2^13,
      rep.int(0, gap)
    )
  }))
  x <- x + runif(length(x), -50, 50)
  tuneR::Wave(round(x), samp.rate=samp.rate, bit=16)
}

test_that("pd_dietrich2004 matches the reference implementation", {
  w <- pulsed_wave()

  for (psi in c(0, 1, 2, 3)) {
    expect_equal(
      pd_dietrich2004(w, U=20, V=80, psi=psi)[c("onsets", "offsets")],
      ref_dietrich(w, U=20, V=80, psi=psi),
      info = paste("psi =", psi)
    )
  }

  for (U in c(20, 40)) {
    for (alpha in c(0.5, 1.4, 3)) {
      expect_equal(
        pd_dietrich2004(w, U=U, V=4*U, alpha=alpha)[c("onsets", "offsets")],
        ref_dietrich(w, U=U, V=4*U, alpha=alpha),
        info = paste("U =", U, "alpha =", alpha)
      )
    }
  }

  # Noise, which gives many short runs rather than a few clean ones.
  set.seed(7)
  n <- tuneR::Wave(round(runif(2000, -2^13, 2^13)), samp.rate=8000, bit=16)
  for (psi in c(0, 1, 2)) {
    expect_equal(
      pd_dietrich2004(n, U=20, V=80, psi=psi)[c("onsets", "offsets")],
      ref_dietrich(n, U=20, V=80, psi=psi),
      info = paste("noise, psi =", psi)
    )
  }
})

test_that("pd_dietrich2004 returns the other documented components", {
  w <- pulsed_wave()
  r <- pd_dietrich2004(w, U=20, V=80)

  expect_named(r, c("theta", "F_a", "F_1", "e_u", "e_v", "onsets", "offsets", "pulse_simple"))
  expect_equal(r$e_u, ste(w, U=20))
  expect_equal(r$e_v, ste(w, U=80))
  expect_equal(r$pulse_simple, r$e_u > r$F_1)
})

test_that("pd_dietrich2004 detects offsets", {
  # Previously the offsets loop wrote to onsets, so offsets was always empty.
  w <- pulsed_wave()
  r <- pd_dietrich2004(w, U=20, V=80)

  expect_true(length(r$onsets) > 0)
  expect_true(length(r$offsets) > 0)
})

test_that("pd_dietrich2004 copes with a wave shorter than the window", {
  w <- tuneR::Wave(rep.int(1, 10), samp.rate=8000, bit=16)

  r <- pd_dietrich2004(w, U=20, V=80)
  expect_equal(r$onsets, integer(0))
  expect_equal(r$offsets, integer(0))
})

test_that("pulseDetection dispatches to the requested method", {
  w <- pulsed_wave()

  expect_equal(
    pulseDetection(w, method="dietrich2004", U=20, V=80),
    pd_dietrich2004(w, U=20, V=80)
  )
  expect_error(pulseDetection(w, method="pertwee"), "No valid method supplied.")
})

# Reference implementation: the original loop-based pd_threshold(), retained so the
# vectorised version can be checked against it.
ref_threshold <- function(wave, threshold=0.2, pd=FALSE, U=1) {
  mag <- c(rep.int(0,U), wave@left)
  if (pd==TRUE) {
    mag <- mag ^ 2
  } else {
    mag <- abs(mag)
  }

  threshold <- threshold* max(mag)
  onsets <- vector(length=length(mag), mode="logical")
  onsets[1:U] <- FALSE
  offsets <- onsets
  for (i in (U + 1):length(mag)) {
    if (mag[i] > threshold & mag[i-1] < threshold) {
      previous <- onsets[(i-U):(i-1)]
      if (length(previous[previous==TRUE]) > 0) {
        onsets[i] <- FALSE
      } else {
        onsets[i] <- TRUE
      }
    } else {
      onsets[i] <- FALSE
    }

    if (mag[i] < threshold & mag[i-1] > threshold) {
      previous <- offsets[(i-U):(i-1)]
      if (length(previous[previous==TRUE]) > 0) {
        offsets[i] <- FALSE
      } else {
        offsets[i] <- TRUE
      }
    } else {
      offsets[i] <- FALSE
    }
  }
  return(list(
    onsets = which(onsets==TRUE),
    offsets = which(offsets==TRUE)
    )
  )
}

test_that("pd_threshold matches the reference implementation", {
  w <- pulsed_wave()
  set.seed(9)
  n <- tuneR::Wave(round(runif(2000, -2^13, 2^13)), samp.rate=8000, bit=16)

  for (wave in list(w, n)) {
    for (U in c(1, 2, 5, 10, 50)) {
      for (threshold in c(0.05, 0.2, 0.5)) {
        for (pd in c(FALSE, TRUE)) {
          #The reference reports positions in the zero-padded magnitudes.
          reference <- ref_threshold(wave, threshold=threshold, pd=pd, U=U)
          expect_equal(
            pd_threshold(wave, threshold=threshold, pd=pd, U=U),
            list(onsets = reference$onsets - U, offsets = reference$offsets - U),
            info = paste("U =", U, "threshold =", threshold, "pd =", pd)
          )
        }
      }
    }
  }
})

test_that("pd_threshold debounces onsets within U samples", {
  # A large U should collapse closely spaced crossings to a single detection.
  set.seed(11)
  n <- tuneR::Wave(round(runif(2000, -2^13, 2^13)), samp.rate=8000, bit=16)

  few <- pd_threshold(n, threshold=0.05, U=200)
  many <- pd_threshold(n, threshold=0.05, U=1)
  expect_true(length(few$onsets) < length(many$onsets))
  expect_true(all(diff(few$onsets) > 200))
})

test_that("pd_threshold detects a pulse starting at the first sample", {
  # The zero padding exists so that a wave which is already above the threshold at
  # its first sample is still reported as an onset.
  w <- tuneR::Wave(rep.int(1, 5), samp.rate=8000, bit=16)

  r <- pd_threshold(w, U=20)
  expect_equal(r$onsets, 1L)
  expect_equal(r$offsets, integer(0))
})

test_that("pd_threshold copes with an empty wave", {
  w <- tuneR::Wave(numeric(0), samp.rate=8000, bit=16)

  r <- pd_threshold(w, U=20)
  expect_equal(r$onsets, integer(0))
  expect_equal(r$offsets, integer(0))
})

test_that("pulseDetection dispatches to pd_threshold", {
  w <- pulsed_wave()

  expect_equal(
    pulseDetection(w, method="threshold", threshold=0.2, U=5),
    pd_threshold(w, threshold=0.2, U=5)
  )
})
