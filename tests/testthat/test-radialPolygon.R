test_that("radialPolygon should not generate errors", {
  emptyDiel()
  expect_silent(radialPolygon(0, 0.5, 0, 0.5))
  expect_silent(radialPolygon(1, 0.5, 1, 0.5))
  expect_silent(radialPolygon(c(0,1), c(0.25, 0.5), c(0, 0.5), c(0.5, 0.75)))
  expect_silent(radialPolygon(c(1,2), c(0.25, 0.5), c(0.25, 0.5), c(0.5, 0.75)))
  expect_silent(radialPolygon(0, 0.5, 0, 0.5, col="pink"))
  expect_silent(radialPolygon(0, 0.5, 0, 0.5, border="orange"))
  expect_silent(radialPolygon(0, 0.5, 0, 0.5, rot=0))
  expect_silent(radialPolygon(0, 0.5, 0, 0.5, angleinc=0.2))
  expect_silent(radialPolygon(0, 0.5, 0, 0.5, reverse=T))
})

test_that("circularise() works as expected", {
  v <- 1:5
  expect_equal(length(circularise(v)), length(v)+1)
  expect_equal(circularise(v)[length(circularise(v))], v[1])
})
