test_that("the environment is built on a Python the packages are published for", {
  skip_if_not_installed("reticulate")
  # The environment used to be built from whatever Python was newest. The
  # machine learning packages it holds lag new releases, so on a runner that had
  # moved to 3.14 or 3.15 numpy was built from source and failed, and the only
  # tensorflow offered was a release candidate, which pip passes over.
  # Suppressed as in .pythonStarter(): looking for a starter reads every Python
  # it can see and warns about any it cannot, such as the stub in WindowsApps.
  starter <- suppressWarnings(reticulate::virtualenv_starter(.pythonVersions()))
  skip_if(is.null(starter), "no Python within the constraint is installed")
  reported <- system2(starter, "--version", stdout=TRUE, stderr=TRUE)
  version <- numeric_version(sub("^Python ", "", reported[[1]]))
  expect_lt(version, numeric_version("3.13"))
})

test_that(".pythonStarter returns a Python that satisfies the constraint", {
  skip_if_not_installed("reticulate")
  skip_if(
    is.null(suppressWarnings(reticulate::virtualenv_starter(.pythonVersions()))),
    "no Python within the constraint is installed"
  )
  starter <- .pythonStarter()
  expect_type(starter, "character")
  expect_true(file.exists(starter))
})

test_that(".pythonStarter says so when no Python fits rather than carrying on", {
  skip_if_not_installed("reticulate")
  # virtualenv_starter() returns NULL rather than raising when nothing matches,
  # so without this the environment would quietly be built on the newest Python
  # again, which is what failed, and the error would come from pip part way
  # through installing.
  expect_null(suppressWarnings(reticulate::virtualenv_starter("<=2.0")))
  expect_error(.pythonStarter("<=2.0"), "No Python <=2.0 was found")
})
