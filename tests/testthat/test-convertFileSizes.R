test_that("Reject invalid input", {
  expect_error(convert2bytes(1, "dog"))
})

test_that("RH is within limits", {
  expect_equal(convert2bytes(8, input="bits"), 1)
  expect_equal(convert2bytes(1, input="bytes"), 1)
})
