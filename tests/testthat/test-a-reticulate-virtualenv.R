test_that("virtualenv installation works", {
  skip_on_cran()
  if (reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    reticulate::virtualenv_remove(envname = "sonicscrewdriver", confirm=FALSE)
  }
  expect_no_warning(pythonInstall(unattended=TRUE))
})
