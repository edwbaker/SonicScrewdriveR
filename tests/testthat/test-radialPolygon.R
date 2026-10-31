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

#radialPolygon() builds its sector from an inner and an outer arc, so the number
#of points polygon() is handed says whether a sector was drawn at all: two
#points are a line and draw nothing visible.
verticesDrawn <- function(expr, env=parent.frame()) {
  n <- NULL
  testthat::local_mocked_bindings(
    polygon = function(x, y, ...) {
      n <<- length(x)
      invisible(NULL)
    },
    .env = env
  )
  force(expr)
  return(n)
}

test_that("a sector narrower than angleinc is still drawn", {
  #angleinc defaults to 0.01 rad, which is 138 seconds of a day. dielPlot()
  #draws its sunrise and sunset bands from the time the sun's disc takes to
  #clear the horizon, about two minutes near the equator, so those bands
  #vanished at low latitudes: seq() returned one angle and the line after it
  #overwrote that with max_a, leaving two points for polygon().
  day <- function(sec) 2*pi*sec/86400
  expect_gte(verticesDrawn(radialPolygon(0, day(60), 1, 2)), 4)
  expect_gte(verticesDrawn(radialPolygon(0, day(120), 1, 2)), 4)
  expect_gte(verticesDrawn(radialPolygon(0, day(137), 1, 2)), 4)
})

test_that("a narrow sector spans the arc it was asked for", {
  #The start angle was the one lost, so the sector has to keep both bounds.
  day <- function(sec) 2*pi*sec/86400
  spanOf <- function(expr, env=parent.frame()) {
    span <- NULL
    testthat::local_mocked_bindings(
      polygon = function(x, y, ...) {
        span <<- diff(range(atan2(y, x)))
        invisible(NULL)
      },
      .env = env
    )
    force(expr)
    return(span)
  }
  expect_equal(spanOf(radialPolygon(0, day(120), 1, 2, rot=0)), day(120))
  expect_equal(spanOf(radialPolygon(0, day(300), 1, 2, rot=0)), day(300))
})

test_that("sectors wider than angleinc are drawn as before", {
  #A guard for the narrow case must not change the arcs that already worked.
  expect_equal(verticesDrawn(radialPolygon(0, pi/2, 1, 2)), 316)
  expect_equal(verticesDrawn(radialPolygon(2*pi-0.2, 0.2, 1, 2)), 82)
  expect_equal(verticesDrawn(radialPolygon(0, 0, 1, 2)), 1258)
})
