test_that("default to air with good known value", {
  expect_equal(frequencySound(343), 1)
})

test_that("naturalFrequency gives expected output", {
  expect_lt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, 0.001)
  expect_gt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, -0.001)
})

test_that("naturalFrequency rejects an overdamped system", {
  # R^2/(4L^2) exceeds 1/(LC), so there is no real natural frequency.
  expect_error(naturalFrequency(L=20, R=0.5), "overdamped")
  expect_error(naturalFrequency(L=20, C=1e5, R=0.5), "overdamped")
  # Critically damped (the discriminant is exactly zero) is still a valid result.
  expect_equal(naturalFrequency(L=1, C=1/4, R=4), 0)
})

test_that("resonantFrequency gives expected output", {
  expect_lt(resonantFrequency(1,1) - 0.15915494, 0.001)
  expect_gt(resonantFrequency(1,1) - 0.15915494, -0.001)
  expect_equal(resonantFrequency(L=1, C=1/4), 1/pi, tolerance = 1e-6)
})

test_that("C defaults to the IUPAC standard pressure in SI units", {
  # 1e5 Pascals, not 100: supplying the documented default explicitly must give
  # the same answer as leaving it out.
  expect_equal(resonantFrequency(L=1), 5.0329212e-4, tolerance = 1e-6)
  expect_equal(resonantFrequency(L=1), resonantFrequency(L=1, C=1e5))
  expect_equal(resonantFrequency(L=1), naturalFrequency(L=1, R=0))
  expect_equal(naturalFrequency(L=20, R=0.01), 1.0527110e-4, tolerance = 1e-6)
  expect_equal(naturalFrequency(L=20, R=0.01), naturalFrequency(L=20, C=1e5, R=0.01))
})

test_that("C can be a vector of capacitances", {
  expect_equal(
    resonantFrequency(L=1, C=c(1, 1/4)),
    c(resonantFrequency(L=1, C=1), resonantFrequency(L=1, C=1/4))
  )
})

test_that("frequencySound divides the speed of sound by the wavelength", {
  # The default test above cannot distinguish speed/wl from wl/speed, as the
  # speed of sound in air is used for both arguments.
  expect_equal(frequencySound(wl=100, s=343), 3.43)
  expect_equal(frequencySound(wl=2, s=660), 330)
})

test_that("frequencySound inverts wavelength", {
  expect_equal(frequencySound(wl=wavelength(1000)), 1000)
  expect_equal(frequencySound(wl=wavelength(50, speed=5941), s=5941), 50)
})
