test_that("the environment is built on a Python the packages are published for", {
  skip_if_not_installed("reticulate")
  # The environment used to be built from whatever Python was newest. The
  # machine learning packages it holds lag new releases, so on a runner that had
  # moved to 3.14 or 3.15 numpy was built from source and failed, and the only
  # tensorflow offered was a release candidate, which pip passes over.
  starter <- reticulate::virtualenv_starter(.pythonVersions())
  skip_if(is.null(starter), "no Python within the constraint is installed")
  reported <- system2(starter, "--version", stdout=TRUE, stderr=TRUE)
  version <- numeric_version(sub("^Python ", "", reported[[1]]))
  expect_lt(version, numeric_version("3.13"))
})

test_that("an impossible constraint is reported rather than ignored", {
  skip_if_not_installed("reticulate")
  # virtualenv_starter() returns NULL rather than raising when nothing matches,
  # so without a check the environment would quietly be built on the newest
  # Python again, which is what failed.
  expect_null(reticulate::virtualenv_starter("<=2.0"))
})
