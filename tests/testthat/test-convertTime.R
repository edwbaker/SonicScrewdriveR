context("convertTime")

test_that("Reject invalid input", {
  expect_error(convert2seconds(1, "dog"))
})

test_that("RH is within limits", {
  expect_equal(convert2seconds(1, input="seconds"), 1)
  expect_equal(convert2seconds(1, input="minutes"), 60)
  expect_equal(convert2seconds(1, input="hours"), 60*60)
  expect_equal(convert2seconds(1, input="days"), 60*60*24)
  expect_equal(convert2seconds(1, input="years"), 60*60*24*365)
})
