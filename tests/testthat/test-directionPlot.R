plotLayers <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[1], character(1))
}

test_that("a plot of the directions given is returned", {
  p <- directionPlot(c(0, 90, 180), c(1, 2, 3))
  expect_s3_class(p, "ggplot")
})

test_that("the directions and speeds given are the data of the plot", {
  p <- directionPlot(c(0, 90, 180), c(1, 2, 3))
  expect_equal(colnames(p$data), c("Angles", "Speeds"))
  expect_equal(p$data$Speeds, c(1, 2, 3, 0))
})

test_that("an origin is added so that the plot is drawn from the centre", {
  p <- directionPlot(c(0, 90), c(1, 2))
  #One row more than was given, at zero on both axes.
  expect_equal(nrow(p$data), 3)
  expect_equal(p$data$Angles[3], 0)
  expect_equal(p$data$Speeds[3], 0)
})

test_that("degrees are converted to the radians the plot is drawn in", {
  p <- directionPlot(c(0, 90, 180), c(1, 2, 3))
  expect_equal(p$data$Angles[1:3], c(0, pi/2, pi))
})

test_that("angles already in radians are taken as they are", {
  p <- directionPlot(c(0, pi/2), c(1, 2), angle.unit="radians")
  expect_equal(p$data$Angles[1:2], c(0, pi/2))
})

test_that("an angle unit the package does not know is refused", {
  expect_error(directionPlot(c(0, 90), c(1, 2), angle.unit="furlongs"))
})

test_that("the mean direction is drawn as an extra segment", {
  with.mean <- directionPlot(c(0, 90), c(1, 2), plot.mean=TRUE)
  without <- directionPlot(c(0, 90), c(1, 2), plot.mean=FALSE)
  expect_equal(length(with.mean$layers), length(without$layers) + 1)
  expect_true(all(plotLayers(without) == "GeomSegment"))
})

test_that("the plot can be drawn", {
  #Building the plot runs the scales and the layers, which a plot that is only
  #assembled never does.
  p <- directionPlot(c(0, 90, 180, 270), c(1, 2, 3, 4))
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_error(ggplot2::ggplot_build(directionPlot(c(0, 90), c(1, 2), plot.mean=FALSE)))
})

test_that("a single direction is plotted rather than refused", {
  p <- directionPlot(45, 2)
  expect_equal(nrow(p$data), 2)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("the plot is drawn without a deprecation or aesthetic warning", {
  expect_no_warning(ggplot2::ggplot_build(directionPlot(c(0, 90, 180), c(1, 2, 3))))
  expect_no_warning(ggplot2::ggplot_build(directionPlot(c(0, 90), c(1, 2), plot.mean=FALSE)))
})

test_that("the mean segment spans the mean direction at the mean speed", {
  p <- directionPlot(c(0, 90), c(1, 3), plot.mean=TRUE)
  mean.layer <- p$layers[[length(p$layers)]]
  expect_equal(nrow(mean.layer$data), 1)
  expect_equal(mean.layer$data$y, 2)
  expect_equal(mean.layer$data$xend, mean(c(0, pi/2)))
  expect_equal(mean.layer$data$x, -mean.layer$data$xend)
})
