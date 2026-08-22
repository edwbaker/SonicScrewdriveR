test_that("default to air with good known value", {
  expect_equal(frequencySound(343), 1)
})

test_that("naturalFrequency gives expected output", {
  expect_lt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, 0.001)
  expect_gt(naturalFrequency(1/2, 1/pi^2, 4) - 0.30775845, -0.001)
  expect_lt(naturalFrequency(L=20,R=0.5) - 0.002950812, 0.001)
  expect_gt(naturalFrequency(L=20,R=0.5) - 0.002950812, -0.001)
})

test_that("resonantFrequency gives expected output", {
  expect_lt(resonantFrequency(1,1) - 0.15915494, 0.001)
  expect_gt(resonantFrequency(1,1) - 0.15915494, -0.001)
  expect_equal(resonantFrequency(L=1, C=1/4), 1/pi, tolerance = 1e-6)
})

test_that("resonantFrequency defaults C to the IUPAC standard pressure", {
  # C=NULL is the sentinel for the default; a non-NULL sentinel would be passed
  # through to naturalFrequency() and used in arithmetic.
  expect_equal(resonantFrequency(L=1), 0.0159154943, tolerance = 1e-6)
  expect_equal(resonantFrequency(L=1), resonantFrequency(L=1, C=100))
  expect_equal(resonantFrequency(L=1), naturalFrequency(L=1, R=0))
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
