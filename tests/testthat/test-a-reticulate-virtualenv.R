test_that("virtualenv installation works", {
  skip_on_cran()
  skip_if_not_installed("reticulate")
  if (reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    reticulate::virtualenv_remove(envname = "sonicscrewdriver", confirm=FALSE)
  }
  expect_no_warning(pythonInstall(unattended=TRUE))
})

test_that(".pipUsable() is FALSE where there is no interpreter to ask", {
  expect_false(.pipUsable(file.path(tempdir(), "no-such-python")))
  expect_false(.pipUsable(""))
  expect_false(.pipUsable(NA_character_))
})

test_that(".pipUsable() follows the exit status of the import", {
  python <- withr::local_tempfile(lines="")
  testthat::local_mocked_bindings(system2=function(...) 0L, .package="base")
  expect_true(.pipUsable(python))
})

test_that(".pipUsable() is FALSE where the import fails", {
  python <- withr::local_tempfile(lines="")
  testthat::local_mocked_bindings(system2=function(...) 1L, .package="base")
  expect_false(.pipUsable(python))
})

test_that(".pipUsable() accepts the environment's own Python", {
  skip_on_cran()
  skip_if_no_virtualenv()
  expect_true(.pipUsable(reticulate::virtualenv_python("sonicscrewdriver")))
})

test_that("pythonInstall() reports an environment whose pip cannot be imported", {
  skip_if_not_installed("reticulate")
  #An environment is reported rather than installed into, so the install is
  #bound to something that says so if it is reached after all.
  testthat::local_mocked_bindings(
    virtualenv_exists=function(...) TRUE,
    virtualenv_python=function(...) file.path(tempdir(), "no-such-python"),
    virtualenv_install=function(...) stop("the install was reached"),
    .package="reticulate"
  )
  expect_error(pythonInstall(unattended=TRUE), "pip in the `sonicscrewdriver` environment cannot be imported")
})
