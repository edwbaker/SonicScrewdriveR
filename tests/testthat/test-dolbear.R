test_that("incorrect input rejected", {
  expect_error(dolbear(), "Dolbear's law calculation requires either n or t to be specified.")
  expect_error(dolbear(t=122, species="Pink salamander"), "dolbear: Pink salamander is not a known species.")
})

test_that("correct input accepted", {
  # 122 C is far outside every range the regressions were fitted over, so these
  # two now warn where they used to be silent. They still return an answer.
  expect_warning(dolbear(t=122), "extrapolated beyond the temperatures")
  expect_no_condition(dolbear(n=122))
  expect_warning(dolbear(t=122, species="Oecanthus fultoni"), "extrapolated beyond")
  expect_no_condition(dolbear(n=122, species="Oecanthus fultoni"))
  expect_no_condition(dolbear(t=25))
  # The function's own example. 6 chirps a minute is about 4.6 C, which is
  # below every range in the table, so this is an extrapolation too.
  expect_warning(dolbear(n=6), "extrapolated beyond the temperatures")
})

test_that("dolbear refuses a rate of chirping or a temperature that cannot be", {
  # Neither argument was checked, so dolbear(n=-50) gave a temperature from a
  # negative rate of chirping and dolbear(t=-40) gave -341.7 chirps a minute.
  expect_error(dolbear(n=-50), "Chirps per minute must not be negative.")
  expect_error(dolbear(n=c(6, -1)), "Chirps per minute must not be negative.")
  expect_error(dolbear(t=-300), "Temperature must be above absolute zero.")
  expect_error(dolbear(n="six"), "Chirps per minute must be numeric.")
  expect_error(dolbear(t="warm"), "Temperature must be numeric.")
})

test_that("dolbear refuses to report a negative rate of chirping", {
  # Every regression in the table crosses zero between about 3.9 and 4.7 C.
  expect_error(dolbear(t=3), "negative rate of chirping at 3 degrees Celsius")
  expect_error(dolbear(t=-40), "negative rate of chirping at -40 degrees Celsius")
  expect_error(dolbear(t=c(25, 3)), "negative rate of chirping at 3 degrees Celsius")
  expect_error(dolbear(t=0, species=NULL), "negative rate of chirping")
})

test_that("dolbear names the populations it has been extrapolated for", {
  # The min and max columns of dolbear.data() were never read by anything.
  expect_warning(
    dolbear(t=10, species="Oecanthus fultoni"),
    "Oecanthus fultoni, Iowa \\(15-25 C\\); Oecanthus fultoni, Ohio \\(18-31 C\\)"
  )
  # Oregon was fitted from 9 C, so 10 C is within its range and it is not named.
  w <- tryCatch(dolbear(t=10), warning=function(w) conditionMessage(w))
  expect_false(grepl("Oregon", w))

  # A temperature inside every range warns about none of them.
  expect_no_warning(dolbear(t=24))
  # 150 chirps a minute is 20-23 C by every one of the three regressions.
  expect_no_warning(dolbear(n=150))

  # The n direction is checked against the same ranges: 0 chirps a minute puts
  # every population near 4 C, below all of them.
  expect_warning(dolbear(n=0), "extrapolated beyond the temperatures")
})
