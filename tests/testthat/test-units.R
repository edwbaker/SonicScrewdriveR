test_that("unit registries list the units the converters accept", {
  expect_setequal(.pressureUnits(), c("Pa", "kPa", "dyne_cm2"))
  expect_setequal(.temperatureUnits(), c("K", "C", "F"))
  expect_setequal(.angleUnits(), c("degrees", "radians"))
  expect_setequal(.fileSizeUnits(), c("bits", "bytes", "kB", "MB", "GB"))
  expect_setequal(.lengthUnits(), c("m", "cm"))
  expect_setequal(.fractionUnits(), c("radians", "fraction"))
  expect_setequal(.wavePositionUnits(), c("samples", "seconds", "minutes", "hours"))
})

test_that("every unit in a registry is accepted by its converter", {
  for (u in .pressureUnits()) {
    expect_no_error(convert2Pascals(1, input.unit=u))
    expect_no_error(convert2dyne_cm2(1, input.unit=u))
  }
  for (u in .temperatureUnits()) {
    expect_no_error(convert2Kelvin(15, input.unit=u))
    expect_no_error(convert2Celsius(15, input.unit=u))
  }
  for (u in .angleUnits()) {
    expect_no_error(convert2degrees(1, input.unit=u))
    expect_no_error(convert2radians(1, input.unit=u))
  }
  for (u in .fileSizeUnits()) {
    expect_no_error(convert2bytes(1, input.unit=u))
  }
  for (u in .lengthUnits()) {
    expect_no_error(wavelength(1000, speed=343, output.unit=u))
  }
  for (u in .wavePositionUnits()) {
    expect_no_error(region(u, from=0, to=1))
  }
})

test_that("an unrecognised unit names the valid units", {
  expect_error(
    convert2Pascals(1, input.unit="psi"),
    "Unknown input.unit to convert2Pascals(): psi. Valid units are: Pa, kPa, dyne_cm2",
    fixed=TRUE
  )
})

test_that("a unit must be a single string", {
  expect_error(convert2Pascals(1, input.unit=c("Pa", "kPa")), "must be a single unit name")
  expect_error(convert2Pascals(1, input.unit=1), "must be a single unit name")
  expect_error(convert2Pascals(1, input.unit=NA_character_), "must be a single unit name")
})

test_that("renamed arguments still work but warn", {
  expect_warning(p <- convert2Pascals(1, input="kPa"), "input argument of convert2Pascals")
  expect_equal(p, 1000)

  expect_warning(d <- convert2dyne_cm2(1, input="Pa"), "input argument of convert2dyne_cm2")
  expect_equal(d, 10)

  expect_warning(k <- convert2Kelvin(0, input="C"), "input argument of convert2Kelvin")
  expect_equal(k, 273.15)

  expect_warning(cc <- convert2Celsius(0, input="K"), "input argument of convert2Celsius")
  expect_equal(cc, -273.15)

  expect_warning(dg <- convert2degrees(pi, input="radians"), "input argument of convert2degrees")
  expect_equal(dg, 180)

  expect_warning(rd <- convert2radians(180, input="degrees"), "input argument of convert2radians")
  expect_equal(rd, pi)

  expect_warning(b <- convert2bytes(8, input="bits"), "input argument of convert2bytes")
  expect_equal(b, 1)

  expect_warning(s <- convert2seconds(1, input="minutes"), "input argument of convert2seconds")
  expect_equal(s, 60)

  expect_warning(ht <- humanTime(60, unit="seconds"), "unit argument of humanTime")
  expect_equal(ht, "1 minute")

  expect_warning(rp <- referencePressure(unit="dyne_cm2"), "unit argument of referencePressure")
  expect_equal(rp, 0.0002)

  expect_warning(ri <- referenceIntensity(unit="watt_cm2"), "unit argument of referenceIntensity")
  expect_equal(ri, 10^-16)

  expect_warning(wl <- wavelength(1000, speed=343, unit="cm"), "unit argument of wavelength")
  expect_equal(wl, 34.3)

  expect_warning(tr <- region(unit="seconds", from=0, to=1), "unit argument of region")
  expect_equal(tr@unit, "seconds")

  expect_warning(df <- dielFraction(0, input="seconds"), "input argument of dielFraction")
  expect_equal(df, 0)

  expect_warning(df2 <- dielFraction(0, input.unit="seconds", unit="fraction"), "unit argument of dielFraction")
  expect_equal(df2, 0)

  expect_warning(yf <- yearlyFraction(0, input="day"), "input argument of yearlyFraction")
  expect_equal(yf, 0)

  expect_warning(yf2 <- yearlyFraction(0, input.unit="day", unit="fraction"), "unit argument of yearlyFraction")
  expect_equal(yf2, 0)
})

test_that("the deprecated T argument of convert2seconds still works", {
  expect_warning(s <- convert2seconds(T=1, input.unit="minutes"), "T argument of convert2seconds")
  expect_equal(s, 60)
})

test_that("the new and deprecated argument names agree", {
  expect_equal(
    suppressWarnings(convert2Pascals(10, input="dyne_cm2")),
    convert2Pascals(10, input.unit="dyne_cm2")
  )
  expect_equal(
    suppressWarnings(wavelength(1000, speed=343, unit="cm")),
    wavelength(1000, speed=343, output.unit="cm")
  )
})

test_that("unit arguments can still be passed positionally", {
  expect_equal(convert2Pascals(1, "kPa"), 1000)
  expect_equal(convert2Kelvin(0, "C"), 273.15)
  expect_equal(region("seconds", 0, 1)@unit, "seconds")
})
