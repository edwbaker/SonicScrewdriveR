test_that("Getting all values", {
  t <- typicalVolume()
  expect_equal(typeof(t), "list")
  expect_equal(ncol(t), 2)
  expect_equal(names(t), c("thing", "dBA"))
})

test_that("Getting specific values", {
  expect_error(typicalVolume("horseradish"), "Thing not found.")
  expect_equal(typicalVolume("steam engine"), 85)
})


