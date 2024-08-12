test_that("plotHMS.at works", {
  expect_equal(length(plotHMS.at()), 25)
  expect_true(is.numeric(plotHMS.at()))
})

test_that("plotHMS.lab works", {
  expect_equal(length(plotHMS.lab()), 25)
  expect_true(is.character(plotHMS.lab()))
})
