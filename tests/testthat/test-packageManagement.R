test_that("packageManagement can identify installed packages", {
  expect_true(package.installed("base"))
})
test_that("packageManagement can identify non-installed packages", {
  expect_false(package.installed("boa constrictor", askInstall = FALSE))
})

test_that("a missing package is offered for installation", {
  asked <- NULL
  installed <- NULL
  local_mocked_bindings(
    askYesNo = function(msg, ...) {
      asked <<- msg
      TRUE
    },
    install.packages = function(pkgs, ...) {
      installed <<- pkgs
      invisible(NULL)
    }
  )
  expect_true(package.installed("boa constrictor"))
  expect_match(asked, "boa constrictor")
  expect_match(asked, "Install now")
  expect_equal(installed, "boa constrictor")
})

test_that("declining the installation is an error rather than a silent FALSE", {
  local_mocked_bindings(
    askYesNo = function(msg, ...) FALSE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_error(package.installed("boa constrictor"), "Missing package not installed.")
})

test_that("cancelling the prompt is treated as declining", {
  #askYesNo() returns NA when the user cancels.
  local_mocked_bindings(
    askYesNo = function(msg, ...) NA,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_error(package.installed("boa constrictor"), "Missing package not installed.")
})

test_that("a package asked for from GitHub is installed from the repository given", {
  skip_if_not_installed("devtools")
  requested <- NULL
  local_mocked_bindings(
    askYesNo = function(msg, ...) TRUE,
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  local_mocked_bindings(
    install_github = function(repo, ...) {
      requested <<- repo
      invisible(NULL)
    },
    .package = "devtools"
  )
  expect_true(package.installed("boa constrictor", method="github", repo="edwbaker"))
  expect_equal(requested, "edwbaker/boa constrictor")
})

test_that("an installed package is reported without asking anything", {
  local_mocked_bindings(
    askYesNo = function(msg, ...) stop("should not be asked"),
    install.packages = function(pkgs, ...) stop("should not be reached")
  )
  expect_true(package.installed("base"))
})
