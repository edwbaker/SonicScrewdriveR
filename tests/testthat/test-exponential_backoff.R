test_that("the first and last tries wait the first and last intervals", {
  expect_equal(exponential_backoff(1, tries=10, first=1, last=10), 1)
  expect_equal(exponential_backoff(10, tries=10, first=1, last=10), 10)
})

test_that("the waits between are spaced geometrically", {
  waits <- vapply(1:5, exponential_backoff, numeric(1), tries=5, first=1, last=16, integer=FALSE)
  expect_equal(waits, c(1, 2, 4, 8, 16))
})

test_that("the wait never decreases as the tries go on", {
  waits <- vapply(1:10, exponential_backoff, numeric(1), integer=FALSE)
  expect_true(all(diff(waits) > 0))
})

test_that("a try beyond the last is given the last wait, not a longer one", {
  last.wait <- exponential_backoff(10, tries=10, first=1, last=10, integer=FALSE)
  expect_equal(exponential_backoff(11, tries=10, first=1, last=10, integer=FALSE), last.wait)
  expect_equal(exponential_backoff(1e6, tries=10, first=1, last=10, integer=FALSE), last.wait)
})

test_that("a single try waits the first interval rather than dividing by zero", {
  #The interval between waits is spread over tries-1 steps, so one try has no
  #interval to spread over.
  expect_equal(exponential_backoff(1, tries=1, first=3, last=10, integer=FALSE), 3)
  expect_equal(exponential_backoff(1, tries=1, first=3, last=10), 3)
  expect_false(is.nan(exponential_backoff(1, tries=1, first=3, last=10, integer=FALSE)))
})

test_that("integer=FALSE keeps the fractional wait that rounding loses", {
  exact <- exponential_backoff(2, tries=4, first=1, last=10, integer=FALSE)
  expect_equal(exact, 10^(1/3))
  expect_equal(exponential_backoff(2, tries=4, first=1, last=10, integer=TRUE), round(exact))
  expect_false(exact == round(exact))
})
