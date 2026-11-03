# Ids worked out with one set of code hashes, so that a test comparing many of
# them hashes each package once.
.idsWith <- function(...) {
  code <- new.env(parent=emptyenv())
  vapply(list(...), .pipelineHash, character(1), code)
}

test_that("an id is a hash of the steps in order", {
  ids <- .idsWith(
    pipeline(normalise(), dutyCycle()),
    pipeline(normalise(), dutyCycle()),
    pipeline(dutyCycle(), normalise()),
    pipeline(normalise()),
    pipeline(normalise(), dutyCycle(limit = 0.2))
  )
  expect_match(ids[1], "^[0-9a-f]{64}$")
  expect_equal(ids[1], ids[2])
  expect_equal(anyDuplicated(ids[-2]), 0)
  expect_equal(.pipelineId(pipeline(normalise(), dutyCycle())), ids[1])
})

test_that("pipelines that compute the same thing share an id", {
  p <- pipeline(normalise(), dutyCycle())
  ids <- .idsWith(
    pipeline(p, beatSpectrum()),
    pipeline(normalise(), dutyCycle(), beatSpectrum()),
    pipeline(normalise(), pipeline(dutyCycle(), beatSpectrum())),
    do.call(pipeline, list(task(normalise), task(dutyCycle), task(beatSpectrum)))
  )
  expect_equal(length(unique(ids)), 1)

  # An argument counts the same however it was given.
  ids <- .idsWith(
    pipeline(dutyCycle(0.2)),
    pipeline(dutyCycle(limit = 0.2)),
    pipeline(dutyCycle(lim = 0.2)),
    pipeline(filterWave(bandpass(1000, 2000))),
    pipeline(filterWave(filt = bandpass(from = 1000, to = 2000)))
  )
  expect_equal(ids[1], ids[2])
  expect_equal(ids[1], ids[3])
  expect_equal(ids[4], ids[5])
})

test_that("anything that could change a number changes the id", {
  ids <- .idsWith(
    pipeline(filterWave(bandpass(1000, 2000))),
    pipeline(filterWave(bandpass(1000, 2001))),
    pipeline(filterWave(bandpass(1000, 2000, wl = 512))),
    pipeline(filterWave(bandpass(1000, 2000), then = dutyCycle())),
    pipeline(filterWave(bandpass(1000, 2000), then = dutyCycle(0.3))),
    pipeline(filterWave(bandpass(1000, 2000)), dutyCycle())
  )
  expect_equal(anyDuplicated(ids), 0)

  # A default the step was not given is part of it, written out whole: two
  # defaults alike for longer than a record keeps of them still differ.
  long <- paste(rep("x", 200), collapse="")
  f1 <- eval(parse(text=sprintf('function(w, a = "%s1") a', long)))
  f2 <- eval(parse(text=sprintf('function(w, a = "%s2") a', long)))
  ids <- .idsWith(pipeline(f1()), pipeline(f2()), pipeline(f1(a = "y")))
  expect_equal(anyDuplicated(ids), 0)
})

test_that("an inner pipeline has the same id alone and nested", {
  inner <- pipeline(dutyCycle(), beatSpectrum())
  outer <- pipeline(normalise(), filterWave(bandpass(1, 2), then = inner))
  code <- new.env(parent=emptyenv())
  expect_equal(
    .pipelineHash(outer$steps[[2]]$then, code),
    .pipelineHash(inner, code)
  )
  # A step given then= hashes the inner id with its own settings.
  changed <- pipeline(normalise(), filterWave(bandpass(1, 2), then = dutyCycle()))
  expect_false(.pipelineHash(outer, code) == .pipelineHash(changed, code))
})

test_that("a function is identified by its package and name, or by its code", {
  expect_equal(.functionIdentity(normalise), "sonicscrewdriver::normalise")
  expect_equal(.functionIdentity(sonicscrewdriver:::.normalise),
               "sonicscrewdriver:::.normalise")
  expect_equal(.functionIdentity(sum), "base::sum")
  expect_equal(.functionIdentity(function(w) w + 1), "function (w) \nw + 1")

  # The user's function is its code: not its name, not whether it was
  # byte-compiled, and not the source it was read from.
  mine <- eval(parse(text="function(w) {\n  # a comment\n  w  +  1\n}", keep.source=TRUE))
  other <- eval(parse(text="function(w) {\n  w + 1\n}", keep.source=FALSE))
  compiled <- compiler::cmpfun(other)
  classed <- structure(other, class=c("special", "function"))
  ids <- .idsWith(
    pipeline(mine()), pipeline(other()), pipeline(compiled()), pipeline(classed()),
    pipeline((function(w) {w + 2})())
  )
  expect_equal(ids[1], ids[2])
  expect_equal(ids[1], ids[3])
  expect_equal(ids[1], ids[4])
  expect_false(ids[1] == ids[5])

  # An S4 generic or method is identified by the function within it, rather
  # than by the object wrapping it.
  expect_match(.functionText(methods::getMethod("addProcess", "TaggedWave")), "^function")
  expect_match(.functionText(addProcess), "^function")
})

test_that("an argument is identified by what it holds", {
  expect_equal(.idValue(1:3), 1:3)
  expect_equal(.idValue(c(a = 1)), c(a = 1))
  expect_null(.idValue(NULL))
  expect_equal(.idValue(list(f = normalise)), list(f = list(.function = "sonicscrewdriver::normalise")))
  expect_equal(.idValue(quote(x + 1)), list(.code = "x + 1"))
  # Attributes are kept, in an order that does not depend on how they were set.
  expect_equal(
    .idValue(structure(1, b = 2, a = 1)),
    .idValue(structure(1, a = 1, b = 2))
  )
  expect_equal(.idValue(factor("a"))$.attributes, list(class = "factor", levels = "a"))
  expect_equal(names(.idValue(data.frame(x = 1))$.value), "x")
  # A filter is its class, and its slots with its function identified.
  filt <- .idValue(bandpass(1000, 2000))
  expect_equal(filt$.slots$func, list(.function = "sonicscrewdriver:::.ffilterWave"))
  expect_equal(filt$.slots$params, list(from = 1000, to = 2000))

  expect_error(.idValue(new.env()), "argument holding an environment")
  expect_error(.idValue(list(1, new.env())), "argument holding an environment")
  expect_error(.idValue(methods::new("externalptr")), "argument holding an externalptr")
})

# covr counts the lines of this package by writing calls to itself into its
# functions, so a coverage run rightly finds covr among the packages they use.
.notCovr <- function(packages) {
  setdiff(packages, "covr")
}

test_that("the packages a step uses are found from its code", {
  expect_equal(
    .notCovr(.stepPackages(pipeline(normalise())$steps[[1]])),
    c("sonicscrewdriver", "digest", "seewave", "tuneR", "R")
  )
  # The function a filter holds is followed, and finds seewave.
  used <- .stepPackages(pipeline(filterWave(bandpass(1, 2)))$steps[[1]])
  expect_true("seewave" %in% used)
  # A function from another package counts its package and is not followed.
  expect_equal(
    .stepPackages(pipeline(seewave::meanspec())$steps[[1]]),
    c("sonicscrewdriver", "seewave", "R")
  )
  # The user's code is followed, through a helper and a default, past the
  # packages that come with R, primitives, and a function this package names
  # as pkg::name; a name this package does not have is passed over.
  helper <- function(w) tuneR::normalize(w)
  mine <- function(w, n = jsonlite::toJSON(1)) {
    stats::median(sum(helper(w)@left))
    sonicscrewdriver:::.ffilterWave
    sonicscrewdriver::noSuchFunction
  }
  expect_setequal(
    .notCovr(.packagesUsed(list(mine))),
    c("jsonlite", "sonicscrewdriver", "tuneR", "seewave")
  )
})

test_that("a step reaching scikit-maad counts it", {
  used <- .packagesUsed(list(maad_acoustic_complexity_index))
  expect_true("scikit-maad" %in% used)
  expect_true("reticulate" %in% used)
  expect_false("scikit-maad" %in% .packagesUsed(list(normalise)))
})

test_that("a package is hashed from the code of its functions and S4 methods", {
  h <- .packageCodeHash("tuneR")
  expect_match(h, "^[0-9a-f]{64}$")
  expect_equal(.packageCodeHash("tuneR"), h)
  expect_false(.packageCodeHash("seewave") == h)
  expect_equal(.packageCodeHash("noSuchPackageAtAll"), "not installed")
})

test_that("each code hash is worked out once for an id, and never kept beyond it", {
  calls <- 0
  local_mocked_bindings(.packageCodeHash = function(package) {
    calls <<- calls + 1
    strrep("0", 64)
  })
  p <- pipeline(normalise(), dutyCycle(), filterWave(bandpass(1, 2), then = normalise()))
  identity <- .pipelineIdentity(p)
  # Each package once, however many steps use it, and R by its version.
  expect_equal(calls, length(identity$code) - 1)
  expect_true(all(c("sonicscrewdriver", "digest", "seewave", "tuneR") %in% names(identity$code)))
  expect_equal(names(identity$code)[1], "sonicscrewdriver")
  expect_equal(identity$code[["R"]], sub("^R version ", "", R.version.string))
  .pipelineIdentity(p)
  expect_equal(calls, 2 * (length(identity$code) - 1))
})

test_that("scikit-maad is hashed from its installed source files", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "features"))
  writeLines("x = 1", file.path(dir, "__init__.py"))
  writeLines("y = 2", file.path(dir, "features", "a.py"))
  writeLines("not source", file.path(dir, "data.csv"))
  local_mocked_bindings(.maadSourceDir = function() dir)
  h <- .pythonCodeHash("scikit-maad")
  expect_match(h, "^[0-9a-f]{64}$")
  writeLines("other", file.path(dir, "data.csv"))
  expect_equal(.pythonCodeHash("scikit-maad"), h)
  writeLines("y = 3", file.path(dir, "features", "a.py"))
  expect_false(.pythonCodeHash("scikit-maad") == h)

  # It is printed after the R packages and before R.
  identity <- .pipelineIdentity(pipeline(maad_acoustic_complexity_index()))
  code <- names(identity$code)
  expect_equal(code[1], "sonicscrewdriver")
  expect_equal(code[length(code) - 1], "scikit-maad")
  expect_equal(code[length(code)], "R")
  expect_equal(identity$code[["scikit-maad"]], .pythonCodeHash("scikit-maad"))
})

test_that("scikit-maad is found where it is installed, and not installed to hash it", {
  local_mocked_bindings(package.installed = function(...) FALSE)
  expect_error(.maadSourceDir(), "scikit-maad is not installed")
  expect_match(
    format(pipeline(maad_acoustic_complexity_index()))[3],
    "^Code: not hashed, as scikit-maad is not installed"
  )
})

test_that("scikit-maad's folder is the one it is imported from", {
  skip_if_not_installed("reticulate")
  local_mocked_bindings(
    package.installed = function(...) TRUE,
    getMaad = function() list(`__file__` = "/somewhere/maad/__init__.py")
  )
  local_mocked_bindings(virtualenv_exists = function(...) TRUE, .package = "reticulate")
  expect_equal(.maadSourceDir(), "/somewhere/maad")
})

test_that("the real scikit-maad can be hashed", {
  skip_on_cran()
  skip_if_no_maad()
  dir <- .maadSourceDir()
  expect_true(file.exists(file.path(dir, "__init__.py")))
  expect_match(.pythonCodeHash("scikit-maad"), "^[0-9a-f]{64}$")
})
