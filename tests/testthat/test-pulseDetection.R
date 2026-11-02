# Reference implementation: Algorithm 1 of Dietrich (2004) transcribed as printed,
# a loop over the samples one at a time with tau counting in samples, written
# against the boundary convention at the top of R/pulseDetection.R. Retained so
# the vectorised version can be checked against it.
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
  #Only the samples both windows cover can be thresholded.
  window <- max(U, V)
  t <- (floor(window/2) + 1):(length(wave) - ceiling(window/2))
  theta <- min(e_u[t]) + gamma*(max(e_u[t]) - min(e_u[t]))
  F_a <- theta + alpha / scaling * e_v
  F_1 <- theta + e_v / scaling

  #The boundary reported is OS, the sample the candidate opened on, not the
  #sample F_a was met on. The scan for onsets starts armed, so a pulse already in
  #progress at the first analysed sample is reported there.
  scan <- function(side, confirm, armed) {
    found <- integer(0)
    tau <- 0
    A <- armed
    OS <- NA_integer_
    for (i in seq_along(side)) {
      if (side[i]) {
        if (tau > 0) {
          if (A == 1) {
            if (confirm[i]) {
              found <- c(found, OS)
              A <- 0
            } else {
              if (tau > psi) {
                tau <- 0
              } else {
                tau <- tau + 1
              }
            }
          }
        } else {
          tau <- tau + 1
          OS <- i
        }
      } else {
        tau <- 0
        A <- 1
      }
    }
    found
  }

  #An offset is an onset of the time reversed wave, and is the sample after the
  #last sample of the pulse. The reversed scan starts disarmed, so a pulse still
  #in progress at the last analysed sample reports no offset.
  side <- e_u[t] > F_1[t]
  confirm <- e_u[t] > F_a[t]
  onsets <- t[scan(side, confirm, armed=1)]
  offsets <- sort(rev(t)[scan(rev(side), rev(confirm), armed=0)] + 1L)

  #A pulse runs from an onset to the first offset after it.
  keptOnsets <- integer(0)
  keptOffsets <- integer(0)
  open <- 0
  for (onset in onsets) {
    if (onset < open) {
      next
    }
    closing <- offsets[offsets > onset]
    keptOnsets <- c(keptOnsets, onset)
    if (length(closing) == 0) {
      break
    }
    keptOffsets <- c(keptOffsets, closing[1])
    open <- closing[1]
  }
  return(list(
    onsets = as.integer(keptOnsets),
    offsets = as.integer(keptOffsets)
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
      pd_dietrich2004(w, U=20, V=80, psi=psi, filter=FALSE)[c("onsets", "offsets")],
      ref_dietrich(w, U=20, V=80, psi=psi),
      info = paste("psi =", psi)
    )
  }

  for (U in c(20, 40)) {
    for (alpha in c(0.5, 1.4, 3)) {
      expect_equal(
        pd_dietrich2004(w, U=U, V=4*U, alpha=alpha, filter=FALSE)[c("onsets", "offsets")],
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
      pd_dietrich2004(n, U=20, V=80, psi=psi, filter=FALSE)[c("onsets", "offsets")],
      ref_dietrich(n, U=20, V=80, psi=psi),
      info = paste("noise, psi =", psi)
    )
  }
})

test_that("pd_dietrich2004 returns the other documented components", {
  w <- pulsed_wave()
  r <- pd_dietrich2004(w, U=20, V=80, filter=FALSE)

  expect_named(r, c("theta", "F_a", "F_1", "e_u", "e_v", "onsets", "offsets", "pulse_simple"))
  expect_equal(r$e_u, ste(w, U=20))
  expect_equal(r$e_v, ste(w, U=80))
  #Nothing outside the samples both windows cover is a pulse, so pulse_simple
  #marks the same samples as pd_simple()'s pulse does.
  analysed <- rep(FALSE, length(w))
  analysed[41:(length(w) - 40)] <- TRUE
  #The comparison is the paper's, which is not a strict one.
  expect_equal(r$pulse_simple, analysed & r$e_u >= r$F_1)
})

test_that("pd_dietrich2004 detects offsets", {
  # Previously the offsets loop wrote to onsets, so offsets was always empty.
  w <- pulsed_wave()
  r <- pd_dietrich2004(w, U=20, V=80, filter=FALSE)

  expect_true(length(r$onsets) > 0)
  expect_true(length(r$offsets) > 0)
})

# A cricket-like wave: pulses of a 5 kHz carrier, which is the sound Table 1's
# U and V are sized for, over a loud low drone standing in for the environment
# the first step of the method exists to remove.
cricketWave <- function(carrier=5000, drone=300, drone.amplitude=0.3,
                        pulse.length=2205, gap=6615, samp.rate=44100, n=44100) {
  tt <- seq_len(n) / samp.rate
  envelope <- rep(0, n)
  starts <- seq(1001, n - pulse.length - gap, by = pulse.length + gap)
  for (s in starts) envelope[s:(s + pulse.length - 1)] <- 1
  list(
    onsets = starts,
    wave = tuneR::Wave(
      sin(2*pi*carrier*tt) * envelope + drone.amplitude * sin(2*pi*drone*tt),
      samp.rate=samp.rate, bit=32, pcm=FALSE
    )
  )
}

peakFrequency <- function(wave) {
  m <- Mod(stats::fft(as.numeric(wave@left)))
  bin <- seq_along(m) - 1
  f <- pmin(bin, length(m) - bin) * wave@samp.rate / length(m)
  f[which.max(m)]
}

test_that("the filter normalises and keeps one band, as equations 1 to 4 say", {
  f <- cricketWave()
  filtered <- .pd_dietrich_filter(f$wave, delta=500, band=c(2000, 11000))

  #Equation 2 finds the carrier, not the louder drone, because the drone is
  #outside the range W is defined over.
  expect_equal(peakFrequency(filtered), 5000)
  expect_equal(peakFrequency(.pd_dietrich_filter(f$wave, delta=500, band=NULL)), 300)
  expect_equal(peakFrequency(.pd_dietrich_filter(f$wave, delta=500, band=c(100, 1000))), 300)

  #Equations 3 and 4 leave nothing outside the passband and, as the passband is
  #symmetric about zero, nothing imaginary either.
  m <- Mod(stats::fft(as.numeric(filtered@left)))
  bin <- seq_along(m) - 1
  freq <- pmin(bin, length(m) - bin) * filtered@samp.rate / length(m)
  expect_lt(sum(m[abs(freq - 5000) >= 500]^2) / sum(m^2), 1e-20)

  #Equation 1 divides by the maximum norm, so the filtered signal is of the
  #order of one whatever the wave was scaled to.
  expect_lt(max(abs(filtered@left)), 1.5)
  loud <- f$wave
  loud@left <- loud@left * 1000
  expect_equal(.pd_dietrich_filter(loud, delta=500, band=c(2000, 11000))@left,
               filtered@left)
})

test_that("normalisation alone moves no boundary", {
  # theta, F_1 and F_a are all linear in the amplitude of the wave, so equation 1
  # cancels out of the segmentation and only the filtering of equations 2 to 4
  # can change an answer.
  f <- cricketWave()
  loud <- f$wave
  loud@left <- loud@left * 1000

  expect_equal(
    pd_dietrich2004(f$wave, filter=FALSE)$onsets,
    pd_dietrich2004(loud, filter=FALSE)$onsets
  )
})

test_that("the filter is what makes the drone ignorable", {
  # The point of the first step of the method. Filtered to the band the song is
  # in, the pulses are found; left unfiltered, or filtered at whatever frequency
  # happens to be loudest, the energy follows the drone and every half cycle of
  # it is reported as a pulse.
  f <- cricketWave()

  expect_equal(length(pd_dietrich2004(f$wave)$onsets), length(f$onsets))
  expect_gt(length(pd_dietrich2004(f$wave, band=NULL)$onsets), 100)

  #Each onset found is near the pulse that produced it, allowing for the half
  #window the energy is displaced by.
  expect_true(all(abs(pd_dietrich2004(f$wave)$onsets - f$onsets) < 120))
})

test_that("a wave that cannot carry the band is refused rather than emptied", {
  # Placing the passband at the edge of the spectrum removes the whole signal
  # and reports no pulses, which reads as a quiet recording rather than as a
  # wave sampled too slowly for the animal being looked for.
  w <- tuneR::Wave(numeric(8000), samp.rate=4000, bit=32, pcm=FALSE)

  expect_error(pd_dietrich2004(w), "only reaches 2000 Hz")
  expect_error(pd_dietrich2004(w), "band=NULL")
  #Both ways out of it work.
  expect_silent(pd_dietrich2004(w, filter=FALSE))
  expect_silent(pd_dietrich2004(w, band=c(100, 1000)))
})

test_that("dt steps the state machine and quantises the boundaries", {
  # Algorithm 1 steps by dt, so it can only report a boundary on that grid.
  f <- cricketWave()

  for (dt in c(10, 30)) {
    r <- pd_dietrich2004(f$wave, dt=dt)
    expect_equal(length(r$onsets), length(f$onsets), info=paste("dt =", dt))
    expect_true(all((r$onsets - 241) %% dt == 0), info=paste("dt =", dt))
    expect_true(all((r$offsets - 241) %% dt == 0), info=paste("dt =", dt))
  }
})

test_that("a step or a band that cannot be used is refused", {
  f <- cricketWave()

  #A fractional step would be truncated one sample at a time into a grid nobody
  #asked for.
  expect_error(pd_dietrich2004(f$wave, dt=2.5), "whole number of samples")
  expect_error(pd_dietrich2004(f$wave, dt=0), "whole number of samples")
  #A band given the wrong way round matches no frequency at all, and the maximum
  #of nothing would have put the passband at zero frequency silently.
  expect_error(pd_dietrich2004(f$wave, band=c(11000, 2000)), "No frequency of the wave lies")
})

test_that("a wave holding a sample that cannot be compared gives no pulses when filtered", {
  # The transform carries one NA into every sample, so there is no filtered
  # signal. The detector reports nothing, as it does for such a wave unfiltered,
  # rather than refusing it or reporting a pulse one end of which is the NA.
  f <- cricketWave()
  f$wave@left[500] <- NA

  r <- pd_dietrich2004(f$wave)
  expect_identical(r$theta, NA_real_)
  expect_length(r$onsets, 0)
  expect_length(r$offsets, 0)
})

# A wave whose pulses rise slowly, so the energy spends several samples between
# F_1 and F_a: the only condition under which the two thresholds can be told
# apart, and so the only one under which psi has anything to bound.
rampedWave <- function(n_pulses=4, pulse.length=400, attack=300, gap=600, samp.rate=8000) {
  envelope <- c(seq(0, 1, length.out=attack), rep.int(1, pulse.length - attack))
  withr::with_seed(3, {
    x <- unlist(lapply(seq_len(n_pulses), function(i) {
      c(sin(seq(0, 40*pi, length.out=pulse.length)) * envelope * 2^13, rep.int(0, gap))
    }))
    tuneR::Wave(round(x + stats::runif(length(x), -30, 30)), samp.rate=samp.rate, bit=16)
  })
}

test_that("pd_dietrich2004 reports the F_1 crossing, not the F_a crossing", {
  # Algorithm 1 reports OS, the sample the candidate opened on, and reaches F_a
  # only to confirm it. Reporting the F_a crossing instead put every boundary
  # late and left psi with nothing to move, as OS is the only thing it moves.
  w <- rampedWave()
  r <- pd_dietrich2004(w, U=20, V=80, psi=5, filter=FALSE)

  expect_true(all(r$e_u[r$onsets] > r$F_1[r$onsets]))
  #The energy has not yet reached F_a at the onsets of the slowly rising pulses,
  #which it would have done had the F_a crossing been the sample reported.
  expect_false(all(r$e_u[r$onsets] > r$F_a[r$onsets]))
})

test_that("psi bounds how far back a pulse is reported from", {
  # psi was threaded through the function but could not change what it returned:
  # every value from 1 upwards gave the same answer, because it only ever moved
  # OS and OS was not what the scan reported.
  w <- rampedWave()
  near <- pd_dietrich2004(w, U=20, V=80, psi=1, filter=FALSE)$onsets
  far <- pd_dietrich2004(w, U=20, V=80, psi=5, filter=FALSE)$onsets

  expect_false(identical(near, far))
  #A candidate may wait longer for F_a, so no onset is reported later than it
  #was with the shorter wait.
  expect_true(all(far <= near))
})

test_that("alpha moves the offsets", {
  # The scan for offsets ran forwards over the samples below F_1 and confirmed
  # them against F_a. F_a is above F_1, so an energy below F_1 is below F_a as
  # well and the confirming test held at every sample of the run: neither alpha
  # nor psi could move an offset, whatever either was set to. Scanning the wave
  # in reverse instead puts both boundaries at F_1 and confirms both at F_a.
  w <- rampedWave()
  r <- pd_dietrich2004(w, U=20, V=80, filter=FALSE)
  t <- .steDefined(length(w), 80)
  expect_equal(sum((r$e_u[t] <= r$F_1[t]) & !(r$e_u[t] <= r$F_a[t])), 0)

  expect_false(identical(
    pd_dietrich2004(w, U=20, V=80, alpha=1.4, filter=FALSE)$offsets,
    pd_dietrich2004(w, U=20, V=80, alpha=8, filter=FALSE)$offsets
  ))
})

test_that("pd_dietrich2004 brackets each pulse at F_1", {
  w <- rampedWave()
  r <- pd_dietrich2004(w, U=20, V=80, filter=FALSE)

  expectPulseConvention(r, "pd_dietrich2004 on a ramped wave")
  expect_length(r$offsets, length(r$onsets))
  #An onset is the first sample of the pulse and an offset the first sample
  #after it, so the energy is above F_1 at the onset and at the sample before
  #the offset, and not above it at the offset.
  expect_true(all(r$e_u[r$onsets] > r$F_1[r$onsets]))
  expect_true(all(r$e_u[r$offsets - 1] > r$F_1[r$offsets - 1]))
  expect_false(any(r$e_u[r$offsets] > r$F_1[r$offsets]))
})

test_that("pd_dietrich2004 copes with a wave shorter than the window", {
  w <- tuneR::Wave(rep.int(1, 10), samp.rate=8000, bit=16)

  r <- pd_dietrich2004(w, U=20, V=80, filter=FALSE)
  expect_equal(r$onsets, integer(0))
  expect_equal(r$offsets, integer(0))
})

test_that("pulseDetection dispatches to the requested method", {
  w <- pulsed_wave()

  expect_equal(
    pulseDetection(w, method="dietrich2004", U=20, V=80, filter=FALSE),
    pd_dietrich2004(w, U=20, V=80, filter=FALSE)
  )
  expect_error(pulseDetection(w, method="pertwee"), "No valid method supplied.")
})

# Reference implementation: a loop over the samples one at a time, written
# against the boundary convention at the top of R/pulseDetection.R. Retained so
# the vectorised version can be checked against it.
ref_threshold <- function(wave, threshold=0.2, pd=FALSE, U=1) {
  if (pd==TRUE) {
    mag <- wave@left ^ 2
  } else {
    mag <- abs(wave@left)
  }
  threshold <- threshold * max(mag)

  onsets <- integer(0)
  offsets <- integer(0)
  inPulse <- FALSE
  for (i in seq_along(mag)) {
    #A sample exactly equal to the threshold is not part of a pulse.
    if (mag[i] > threshold) {
      if (!inPulse) {
        onsets <- c(onsets, i)
        inPulse <- TRUE
      }
    } else {
      if (inPulse) {
        offsets <- c(offsets, i)
        inPulse <- FALSE
      }
    }
  }

  #A pulse is kept or discarded whole.
  kept <- integer(0)
  last <- -Inf
  for (k in seq_along(onsets)) {
    if (onsets[k] - last > U) {
      kept <- c(kept, k)
      last <- onsets[k]
    }
  }
  return(list(
    onsets = as.integer(onsets[kept]),
    offsets = as.integer(offsets[kept[kept <= length(offsets)]])
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
          expect_equal(
            pd_threshold(wave, threshold=threshold, pd=pd, U=U),
            ref_threshold(wave, threshold=threshold, pd=pd, U=U),
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

test_that("the energy detectors cope with an empty wave", {
  # ste() of no samples is a zero-length vector, so min() and max() warned and
  # returned Inf and -Inf, leaving theta as NaN. There is no energy to threshold,
  # so theta is undefined and the result is empty, as pd_threshold() already gave.
  w <- tuneR::Wave(numeric(0), samp.rate=8000, bit=16)

  for (detector in c("simple", "dietrich2004")) {
    expect_silent(r <- pulseDetection(w, method=detector))
    expect_identical(r$theta, NA_real_, info=detector)
    expect_length(r$onsets, 0)
    expect_length(r$offsets, 0)
  }
})

test_that("the energy detectors have no threshold for a wave shorter than the window", {
  # ste() returns a zero for every sample of such a wave rather than nothing at
  # all, and theta was taken from those zeroes: a wave that holds no window at
  # all was given a noise floor of zero, which is the same reading of the padding
  # that reported stationary noise as one long pulse.
  w <- tuneR::Wave(rep.int(1, 50), samp.rate=8000, bit=16)

  for (detector in c("simple", "dietrich2004")) {
    expect_silent(r <- pulseDetection(w, method=detector))
    expect_identical(r$theta, NA_real_, info=detector)
    expect_length(r$onsets, 0)
    expect_length(r$offsets, 0)
  }
})

test_that("pulseDetection dispatches to pd_threshold", {
  w <- pulsed_wave()

  expect_equal(
    pulseDetection(w, method="threshold", threshold=0.2, U=5),
    pd_threshold(w, threshold=0.2, U=5)
  )
})

#A quiet wave with a loud burst in it, which the detectors should find.
burstWave <- function(n=8000, samp.rate=4000) {
  samples <- rep(0, n)
  samples[2001:3000] <- 10000
  tuneR::Wave(samples, samp.rate=samp.rate, bit=16)
}

test_that("a method the package does not have is refused", {
  expect_error(pulseDetection(burstWave(), method="telepathy"), "No valid method supplied.")
})

test_that("the simple method is the default", {
  wave <- burstWave()
  expect_equal(pulseDetection(wave), pd_simple(wave))
  expect_equal(pulseDetection(wave, method="simple"), pd_simple(wave))
})

test_that("each named method is dispatched to its own detector", {
  wave <- burstWave()
  #burstWave() is sampled at 4 kHz and so reaches 2 kHz, which is the bottom of
  #the frequency range pd_dietrich2004() filters to by default, so the wave is
  #taken as it is here rather than through a passband it cannot carry.
  expect_equal(
    pulseDetection(wave, method="dietrich2004", filter=FALSE),
    pd_dietrich2004(wave, filter=FALSE)
  )
  expect_equal(pulseDetection(wave, method="threshold"), pd_threshold(wave))
})

test_that("arguments are passed through to the detector", {
  wave <- burstWave()
  expect_equal(pulseDetection(wave, method="simple", U=60), pd_simple(wave, U=60))
  expect_false(identical(pulseDetection(wave, U=60), pulseDetection(wave, U=240)))
})

test_that("a stereo wave is analysed one channel at a time", {
  wave <- burstWave()
  #The right channel is silent, so the two channels give different answers and
  #analysing only the left would not show it.
  stereo <- tuneR::Wave(
    left=wave@left, right=rep(0, length(wave@left)),
    samp.rate=wave@samp.rate, bit=16
  )
  res <- pulseDetection(stereo)
  expect_length(res, 2)
  expect_equal(res[[1]], pd_simple(wave))
  expect_false(any(res[[2]]$pulse))
})

test_that("a multi-channel wave is analysed one channel at a time", {
  wave <- burstWave()
  mc <- tuneR::WaveMC(
    data = cbind(wave@left, rep(0, length(wave@left)), wave@left),
    samp.rate = wave@samp.rate, bit = 16
  )
  res <- pulseDetection(mc)
  expect_length(res, 3)
  expect_equal(res[[1]], res[[3]])
  expect_false(any(res[[2]]$pulse))
})

test_that("pd_threshold refuses a wave holding NA samples", {
  # max(mag) was NA, so every comparison was NA and which() dropped them all:
  # a clean "no pulses" answer for a wave with an obvious pulse in it.
  # The NA goes into the slot rather than through Wave(), so the test is of the
  # detector and not of tuneR's validity checking.
  w <- tuneR::Wave(c(rep.int(0, 51), rep.int(1, 20), rep.int(0, 50)),
                   samp.rate=8000, bit=16)
  w@left[51] <- NA

  expect_error(pd_threshold(w, threshold=0.5), "NA samples")
  expect_error(pulseDetection(w, method="threshold", threshold=0.5), "NA samples")
})
