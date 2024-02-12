test_that("Radar power equation gives correct result", {
  expect_equal(radarPower(12, 20, 0.05), 1.889738e-09)
  expect_equal(radarPower(12, 20, 0.05, G_t=1.2, G_r=1.5, wl=0.045), 6.888096e-12)
})

test_that("Radar range equation gives correct result", {
  expect_equal(radarRange(2, 343), 343)
})
