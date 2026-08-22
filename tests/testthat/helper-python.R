# The Python side of the package lives in a virtualenv that is created by
# pythonInstall(), which the tests only run off CRAN. Under `R CMD check` that
# installation is skipped, so the environment and the modules in it are absent
# and every test that reaches Python has to skip rather than fail. The check is
# made against the environment itself, not against whether the installation test
# ran, so that it is also right where the environment exists but a single module
# failed to install.
skip_if_no_virtualenv <- function() {
  if (!requireNamespace("reticulate", quietly=TRUE)) {
    testthat::skip("reticulate is not available for testing")
  }
  if (!reticulate::virtualenv_exists(envname = "sonicscrewdriver")) {
    testthat::skip("sonicscrewdriver Python virtualenv not available for testing")
  }
  reticulate::use_virtualenv("sonicscrewdriver")
}

skip_if_no_py_module <- function(module) {
  skip_if_no_virtualenv()
  #py_module_available() starts Python, which can itself fail where the
  #environment is broken rather than missing.
  available <- tryCatch(reticulate::py_module_available(module), error=function(e) FALSE)
  if (!available) {
    testthat::skip(paste(module, "not available for testing"))
  }
}

skip_if_no_maad <- function() {
  skip_if_no_py_module("maad")
}
