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

test_that("error in soundSpeed_vague for unknown medium", {
  expect_error(soundSpeed(medium="pig semen"))
})
