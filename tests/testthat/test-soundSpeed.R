context("soundSpeed")

test_that("default value is for air, and correct", {
  expect_equal(soundSpeedMedium(), 343)
})

test_that("error in soundSpeed_vague for unknown medium", {
  expect_error(soundSpeed_vague(medium="pig semen"))
})
