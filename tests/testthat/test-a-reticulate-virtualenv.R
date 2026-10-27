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

test_that("reticulate is said to be needed when it is missing", {
  local_mocked_bindings(package.installed = function(name, ...) FALSE)
  expect_error(
    pythonInstall(unattended=TRUE),
    "The reticulate package is required to create the `sonicscrewdriver` environment.",
    fixed = TRUE
  )
})

test_that("an environment is not created behind the user's back", {
  #Outside an interactive session there is nobody to ask, so an installation that
  #was not asked for is refused rather than going ahead.
  expect_false(interactive())
  expect_error(
    pythonInstall(),
    "The `sonicscrewdriver` environment is required to use maad functions in sonicscrewdriver.",
    fixed = TRUE
  )
})

test_that("a locale is only set where the environment names none", {
  withr::with_envvar(c(LC_ALL=NA, LC_CTYPE=NA, LANG="en_GB.UTF-8"), {
    #A locale the user has chosen is left alone.
    expect_null(sonicscrewdriver:::.useUtf8Locale())
  })
})

test_that("nothing is set where no UTF-8 locale can be found", {
  local_mocked_bindings(.utf8LocaleName = function() NULL)
  withr::with_envvar(c(LC_ALL=NA, LC_CTYPE=NA, LANG=NA), {
    expect_null(sonicscrewdriver:::.useUtf8Locale())
    expect_equal(Sys.getenv("LC_CTYPE"), "")
  })
})

test_that("a UTF-8 locale is set where the environment names none", {
  withr::with_envvar(c(LC_ALL=NA, LC_CTYPE=NA, LANG=NA), {
    set <- sonicscrewdriver:::.useUtf8Locale()
    expect_true(grepl("UTF-8", set, fixed=TRUE))
    expect_equal(Sys.getenv("LC_CTYPE"), set)
  })
})

test_that("the locale chosen is one the machine accepts", {
  name <- sonicscrewdriver:::.utf8LocaleName()
  skip_if(is.null(name), "no UTF-8 locale is available on this machine")
  expect_true(grepl("UTF-8", name, fixed=TRUE))
  original <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", original)))
  expect_true(nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", name))))
})
