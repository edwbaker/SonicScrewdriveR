test_that("maad_power2dB works", {
  maad <- getMaad()
  expect_equal(maad_power2dB(1, maad=maad), 0)
  expect_equal(maad_power2dB(3), 10*log10(3))

  t <- rep_len(1, 10)
  expect_true(all(maad_power2dB(t, maad=maad) == 0))
  t <- as.matrix(t, ncol=2)
  expect_true(all(maad_power2dB(t, maad=maad) == 0))
  t <- as.data.frame(t)
  expect_true(all(maad_power2dB(t, maad=maad) == 0))
})
