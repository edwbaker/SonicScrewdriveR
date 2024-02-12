test_that("packageManagement can identify installed packages", {
  expect_true(package.installed("base"))
})
test_that("packageManagement can identify non-installed packages", {
  expect_false(package.installed("boa constrictor", askInstall = FALSE))
})
