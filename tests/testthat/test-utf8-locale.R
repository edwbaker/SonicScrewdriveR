test_that(".utf8LocaleName names a UTF-8 locale without changing the one in use", {
  before <- Sys.getlocale("LC_CTYPE")
  name <- .utf8LocaleName()
  expect_equal(Sys.getlocale("LC_CTYPE"), before)

  skip_if(is.null(name), "No UTF-8 locale available on this system")
  expect_match(name, "UTF-8$")
})

test_that(".useUtf8Locale leaves a locale named in the environment alone", {
  # Whatever the user has asked for, including a locale that is not UTF-8.
  for (locale in c("en_GB.UTF-8", "C")) {
    withr::with_envvar(c(LC_ALL=NA, LC_CTYPE=NA, LANG=locale), {
      expect_null(.useUtf8Locale())
      expect_equal(Sys.getenv("LC_CTYPE"), "")
    })
  }
  withr::with_envvar(c(LC_ALL="C", LC_CTYPE=NA, LANG=NA), {
    expect_null(.useUtf8Locale())
    expect_equal(Sys.getenv("LC_CTYPE"), "")
  })
})

test_that(".useUtf8Locale sets LC_CTYPE where the environment names no locale", {
  before <- Sys.getlocale("LC_CTYPE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", before)))

  withr::with_envvar(c(LC_ALL=NA, LC_CTYPE=NA, LANG=NA), {
    # The C locale is what Python is left in when nothing names another, and is
    # the state the encoding has to be recovered from.
    suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
    skip_if(is.null(.utf8LocaleName()), "No UTF-8 locale available on this system")

    target <- .useUtf8Locale()
    expect_match(target, "UTF-8$")
    expect_equal(Sys.getenv("LC_CTYPE"), target)
    expect_true(l10n_info()[["UTF-8"]])
  })
})
