test_that("packageManagement can identify installed packages", {
  expect_true(package.installed("base"))
})
test_that("packageManagement can identify non-installed packages", {
  expect_false(package.installed("boa constrictor", askInstall = FALSE))
})

test_that("nothing is installed where there is no one to ask", {
  #askYesNo() returns its default, which is TRUE, where nothing answers it, so
  #a non-interactive session went straight on to install.packages() and wrote
  #into the library unasked: R CMD check, CI and cron runs all attempted
  #installs, and a plotting call wrote to the user's library as a side effect
  #of drawing.
  local_mocked_bindings(
    .canAsk = function() FALSE,
    askYesNo = function(msg, ...) stop("should not be asked"),
    install.packages = function(pkgs, ...) stop("should not be installed")
  )
  #askInstall is TRUE by default, which is what the eleven callers use.
  expect_false(package.installed("boa constrictor"))
})

test_that("a missing package is offered for installation", {
  asked <- NULL
  installed <- NULL
  arrived <- FALSE
  local_mocked_bindings(
    .canAsk = function() TRUE,
    .packageUsable = function(name) arrived,
    askYesNo = function(msg, ...) {
      asked <<- msg
      TRUE
    },
    install.packages = function(pkgs, ...) {
      installed <<- pkgs
      arrived <<- TRUE
      invisible(NULL)
    }
  )
  expect_true(package.installed("boa constrictor"))
  expect_match(asked, "boa constrictor")
  expect_match(asked, "Install now")
  expect_equal(installed, "boa constrictor")
})

test_that("an install that produced nothing is not reported as a success", {
  #install.packages() returns nothing whether or not it installed anything and
  #reports a failure it noticed as a warning, so TRUE was returned for an
  #install that had not happened and the caller went on to use a package that
  #was not there.
  local_mocked_bindings(
    .canAsk = function() TRUE,
    .packageUsable = function(name) FALSE,
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) invisible(NULL)
  )
  expect_false(package.installed("boa constrictor"))
})

test_that("declining the installation is an error rather than a silent FALSE", {
  local_mocked_bindings(
    .canAsk = function() TRUE,
    askYesNo = function(msg, ...) FALSE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_error(package.installed("boa constrictor"), "Missing package not installed.")
})

test_that("cancelling the prompt is treated as declining", {
  #askYesNo() returns NA when the user cancels.
  local_mocked_bindings(
    .canAsk = function() TRUE,
    askYesNo = function(msg, ...) NA,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_error(package.installed("boa constrictor"), "Missing package not installed.")
})

test_that("a package asked for from GitHub is installed from the repository given", {
  skip_if_not_installed("devtools")
  requested <- NULL
  arrived <- FALSE
  local_mocked_bindings(
    .canAsk = function() TRUE,
    #devtools is there, the package asked for is not until it is installed.
    .packageUsable = function(name) if (name == "devtools") TRUE else arrived,
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  local_mocked_bindings(
    install_github = function(repo, ...) {
      requested <<- repo
      arrived <<- TRUE
      invisible(NULL)
    },
    .package = "devtools"
  )
  expect_true(package.installed("boa constrictor", method="github", repo="edwbaker"))
  expect_equal(requested, "edwbaker/boa constrictor")
})

test_that("a github install that produced nothing is not reported as a success", {
  skip_if_not_installed("devtools")
  local_mocked_bindings(
    .canAsk = function() TRUE,
    .packageUsable = function(name) name == "devtools",
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  local_mocked_bindings(
    install_github = function(repo, ...) invisible(NULL),
    .package = "devtools"
  )
  expect_false(package.installed("boa constrictor", method="github", repo="edwbaker"))
})

test_that("a github install cannot go ahead without devtools", {
  #The tripwire needs devtools there to be stood in for, which is not a
  #contradiction: .packageUsable() is what decides whether the code can see it.
  skip_if_not_installed("devtools")
  local_mocked_bindings(
    .canAsk = function() TRUE,
    .packageUsable = function(name) FALSE,
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) invisible(NULL)
  )
  local_mocked_bindings(
    install_github = function(repo, ...) stop("should not be reached"),
    .package = "devtools"
  )
  #devtools is asked for in its turn, and its install produces nothing either.
  expect_false(package.installed("boa constrictor", method="github", repo="edwbaker"))
})

test_that("a method that installs nothing is refused by name", {
  #Anything other than "github" installed nothing and returned NULL, which a
  #caller testing it with if () could not use.
  local_mocked_bindings(
    .canAsk = function() TRUE,
    .packageUsable = function(name) FALSE,
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_error(
    package.installed("boa constrictor", method="bitbucket"),
    "Unknown method for installing a package: bitbucket"
  )
})

test_that("an installed package is reported without asking anything", {
  local_mocked_bindings(
    askYesNo = function(msg, ...) stop("should not be asked"),
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_true(package.installed("base"))
})

test_that(".canAsk() asks interactive() and nothing else", {
  #Every test above stands in for it, so this is the one place it is read. It
  #exists only because local_mocked_bindings() cannot stand in for a base
  #function - it fails with "Can't find binding for `interactive`" - so what it
  #has to be is that base function.
  expect_identical(.canAsk(), interactive())
})
