test_that("scikit-maad installation works", {
  skip_on_cran()
  if (reticulate::virtualenv_exists(envname = "ssd_scikit-maad")) {
    reticulate::virtualenv_remove(envname = "ssd_scikit-maad")
  }
  expect_no_warning(maadInstall(unattended=TRUE))
  expect_no_warning(getMaad())
})
