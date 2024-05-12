test_that("incorrect input rejected", {
  expect_error(dolbear(), "Dolbear's law calculation requires either n or t to be specified.")
  expect_error(dolbear(t=122, species="Pink salamander"), "dolbear: Pink salamander is not a known species.")
})

test_that("correct input accepted", {
  expect_no_condition(dolbear(t=122))
  expect_no_condition(dolbear(n=122))
  expect_no_condition(dolbear(t=122, species="Oecanthus fultoni"))
  expect_no_condition(dolbear(n=122, species="Oecanthus fultoni"))
})
