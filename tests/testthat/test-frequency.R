test_that("default to air with good known value", {
  expect_equal(frequencySound(343), 1)
})

test_that("naturalFrequency gives expected output", {
  expect_lt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, 0.001)
  expect_gt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, -0.001)
})

test_that("naturalFrequency defaults C to the IUPAC standard pressure in SI units", {
  # 1e5 Pascals, not 100: supplying the documented default explicitly must give
  # the same answer as leaving it out.
  expect_equal(naturalFrequency(L=20, R=0.01), 1.0527110e-4, tolerance = 1e-6)
  expect_equal(naturalFrequency(L=20, R=0.01), naturalFrequency(L=20, C=1e5, R=0.01))
  expect_equal(resonantFrequency(L=1), 5.0329212e-4, tolerance = 1e-6)
  expect_equal(resonantFrequency(L=1), resonantFrequency(L=1, C=1e5))
})

test_that("naturalFrequency rejects an overdamped system", {
  expect_error(naturalFrequency(L=20, R=0.5), "overdamped")
  expect_error(naturalFrequency(L=20, C=1e5, R=0.5), "overdamped")
  # Critically damped (discriminant exactly zero) is still a valid result.
  expect_equal(naturalFrequency(L=1, C=1/4, R=4), 0)
})

test_that("resonantFrequency gives expected output", {
  expect_lt(resonantFrequency(1,1) - 0.15915494, 0.001)
  expect_gt(resonantFrequency(1,1) - 0.15915494, -0.001)
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
