test_that("error in soundSpeed for unknown medium", {
  expect_error(soundSpeed(medium="pig semen"))
})

test_that("default value is for air, and correct", {
  expect_true(is.numeric(soundSpeed()))
  expect_equal(soundSpeed(), 343)
})

test_that("all works as a medium", {
  expect_type(soundSpeed(medium="all"), "list")
  expect_equal(ncol(soundSpeed(medium="all")), 2)
})

test_that("specific medium works", {
  expect_equal(soundSpeed(medium="steel"), 5941)
})

test_that("frequency and wavelength calculation works", {
  expect_equal(soundSpeed(f=330, wl=2), 660)
})

test_that("bulk modulus and density calculation works", {
  expect_equal(soundSpeed(bulkModulus =2.02e5, density=2), sqrt(1.01e5))
})

test_that("cramer method works as expected", {
  expect_equal(soundSpeed(method="cramer", temp=14, pressure=3, RH=10), 342.648156, tolerance=1e-6)
  expect_equal(
    soundSpeed(method="cramer", temp=14, temp.unit="C", pressure=3, pressure.unit="kPa", RH=10),
    soundSpeed(method="cramer", temp=14, pressure=3, RH=10)
  )
})

test_that("cramer method reproduces published speeds of sound in dry air", {
  # Cramer (1993) is quoted for dry air at 101.325 kPa with 400 ppm CO2. Pinning
  # these rather than the output of an arbitrary call means the expectations come
  # from the paper. The default MoleFracCO2 is used, so a typo in it fails here.
  dry <- function(t) {
    .soundSpeed_cramer1993(t, pressure=101325, pressure.unit="Pa", RH=0)
  }
  expect_equal(dry(0), 331.45, tolerance=1e-4)
  expect_equal(dry(20), 343.36, tolerance=1e-4)
})

test_that("cramer method defaults to the CO2 mole fraction used in the paper", {
  # Written as 400^-6 (2.4e-16) rather than 400e-6 until corrected.
  expect_equal(eval(formals(.soundSpeed_cramer1993)$MoleFracCO2), 400e-6)
})

test_that("cramer method matches the coefficients as published", {
  # Cramer (1993), JASA 93(5):2510-2516, approximate expression. Written out here
  # independently of the implementation so that a typo in either one shows up.
  reference <- function(t, p, xw, xc) {
    a <- c(331.5024, 0.603055, -0.000528,
           51.471935, 0.1495874, -0.000782,
           -1.82e-7, 3.73e-8, -2.93e-10,
           -85.20931, -0.228525, 5.91e-5,
           -2.835149, -2.15e-13, 29.179762, 0.000486)
    a[1] + a[2]*t + a[3]*t^2 +
      (a[4] + a[5]*t + a[6]*t^2) * xw +
      (a[7] + a[8]*t + a[9]*t^2) * p +
      (a[10] + a[11]*t + a[12]*t^2) * xc +
      a[13]*xw^2 + a[14]*p^2 + a[15]*xc^2 + a[16]*xw*p*xc
  }
  # The mole fraction of water vapour, as the implementation derives it, so that
  # this compares the polynomial rather than the vapour pressure model.
  moleFracH2O <- function(t, p, RH) {
    K <- t + 273.15
    enh <- pi*1e-8*p + 1.00062 + t^2*5.6e-7
    psv <- exp(K^2*1.2378847e-5 - 1.9121316e-2*K) * exp(33.93711047 - 6.3431645e3/K)
    return(RH*enh*psv/p/100)
  }

  grid <- expand.grid(t=c(0, 10, 20, 30), p=c(75000, 100000, 101325), RH=c(0, 50, 100))
  for (i in seq_len(nrow(grid))) {
    t <- grid$t[i]; p <- grid$p[i]; RH <- grid$RH[i]
    expect_equal(
      .soundSpeed_cramer1993(t, pressure=p, pressure.unit="Pa", RH=RH, MoleFracCO2=400e-6),
      reference(t, p, moleFracH2O(t, p, RH), 400e-6),
      tolerance = 1e-10
    )
  }
})

test_that("seewave method works as expected", {
  expect_equal(soundSpeed(method="seewave", temp=20), 343.4)
  expect_error(soundSpeed(method="seewave"), "Temperature must be specified.")
})
