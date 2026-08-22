test_that("dielPositions starts at midnight and does not repeat it", {
  # Counting from one left out the position for midnight and put an entry at
  # 2*pi, which is the same place as zero.
  h <- dielPositions("hours")
  expect_equal(length(h), 24)
  expect_equal(h[1], 0)
  expect_true(all(h < 2*pi))
  expect_equal(diff(h), rep(2*pi/24, 23))

  m <- dielPositions("minutes")
  expect_equal(length(m), 24*60)
  expect_equal(m[1], 0)
  expect_true(all(m < 2*pi))
})

test_that("dielPositions rejects an unknown format", {
  expect_error(dielPositions("3hours"), "Unknown format for dielPositions")
})

test_that("yearlyPositions puts mid-month labels inside their month", {
  # The last month was measured against a 360 day year, so the December label sat
  # outside December.
  for (year in c(2023, 2024)) {
    starts <- yearlyPositions(year, "months")
    mids <- yearlyPositions(year, "mid-months")
    expect_true(all(mids > starts))
    expect_true(all(mids[1:11] < starts[2:12]))
    expect_true(all(mids < 2*pi))
  }
})

test_that("radialPolygon draws a whole ring when it starts where it ends", {
  # The angles were left equal, which collapsed the grid to one value and drew a
  # two point polygon, which with the default border is invisible.
  vertices <- NULL
  f <- radialPolygon
  e <- new.env(parent=environment(f))
  assign("polygon", function(x, y, ...) { vertices <<- length(x); invisible(NULL) }, envir=e)
  environment(f) <- e

  pdf(NULL)
  on.exit(dev.off())
  plot(NA, xlim=c(-2,2), ylim=c(-2,2))
  f(0, 0, 1, 2)
  closed <- vertices
  f(0, 2*pi, 1, 2)
  expect_equal(closed, vertices)
  expect_gt(closed, 100)
})

test_that("dielRings fills the region however many rings there are", {
  # Ring thickness was fixed at 0.1 while the spacing came from the limits, so
  # more than ten rings overlapped and ran past the outer limit.
  drawn <- list()
  f <- dielRings
  e <- new.env(parent=environment(f))
  assign("radialPolygon", function(a1, a2, r1, r2, ...) {
    drawn[[length(drawn)+1]] <<- c(r1, r2); invisible(NULL)
  }, envir=e)
  assign("legend", function(...) invisible(NULL), envir=e)
  environment(f) <- e

  f(paste0("r", 1:12), rep("0000", 12), rep("1200", 12), limits=c(1, 2))
  radii <- do.call(rbind, drawn)
  expect_equal(nrow(radii), 12)
  expect_true(all(radii <= 2 + 1e-9))
  expect_true(all(radii >= 1 - 1e-9))
  # Each ring begins where the one before it ended.
  expect_equal(radii[-1, 1], radii[-nrow(radii), 2])
})

test_that("dielRings draws nothing when given no rings", {
  expect_silent(dielRings(character(0), character(0), character(0), legend=FALSE))
})

test_that("dielRings and dielHistogram take the plot rotation", {
  # Without it any overlay was drawn at the default rotation and did not line up
  # with a dielPlot() drawn at another.
  expect_true("rot" %in% names(formals(dielRings)))
  expect_true("rot" %in% names(formals(dielHistogram)))
})
