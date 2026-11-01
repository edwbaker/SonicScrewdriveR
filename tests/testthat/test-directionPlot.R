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

test_that("the mean segment points at the mean direction, at the mean speed", {
  p <- directionPlot(c(0, 90), c(1, 3), plot.mean=TRUE)
  mean.layer <- p$layers[[length(p$layers)]]
  expect_equal(nrow(mean.layer$data), 1)
  # A spoke from the centre, as each direction is drawn, rather than an arc
  # either side of north.
  expect_equal(mean.layer$data$y, 0)
  expect_equal(mean.layer$data$yend, 2)
  expect_equal(mean.layer$data$x, pi/4)
  expect_equal(mean.layer$data$xend, mean.layer$data$x)
})

test_that("the mean of a set of directions is the direction of its resultant", {
  # mean(abs(angles)) was the mean distance from north, drawn either side of it,
  # so a set of easterly directions and the matching westerly set gave the same
  # answer and neither pointed where the directions were.
  meanOf <- function(d) {
    p <- directionPlot(d, rep(1, length(d)), plot.mean=TRUE)
    p$layers[[length(p$layers)]]$data$x
  }
  expect_equal(meanOf(c(80, 100)), pi/2)
  expect_equal(meanOf(c(260, 280)), -pi/2)
  # The arithmetic mean of 350 and 10 is 180, due south.
  expect_equal(meanOf(c(350, 10)), 0)
  expect_equal(meanOf(c(270, 350)), convert2radians(310) - 2*pi)
})

test_that("directions that cancel have no mean direction to draw", {
  # The resultant has no direction, and atan2(0, 0) is 0, so the segment would
  # have been drawn pointing north on no evidence.
  without <- directionPlot(c(0, 180), c(1, 1), plot.mean=FALSE)
  for (d in list(c(0, 180), c(90, 270))) {
    p <- directionPlot(d, c(1, 1), plot.mean=TRUE)
    expect_equal(length(p$layers), length(without$layers))
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("every direction given is drawn, not only the eastern half", {
  # The scale ran over (-pi, pi] while a bearing converted to radians runs over
  # [0, 2*pi), and ggplot2 drops a value outside a scale's limits without a
  # word, so a wind rose quietly showed only half of itself.
  p <- directionPlot(c(10, 90, 181, 270, 350), c(1, 2, 3, 4, 5))
  x <- ggplot2::ggplot_build(p)$data[[1]]$x
  # The five directions given, and the origin row that draws them from the
  # centre.
  expect_equal(length(x), 6)
  expect_equal(sum(is.na(x)), 0)
})

test_that("a bearing is placed where that point of the compass is", {
  p <- directionPlot(c(0, 90, 180, 270, 360), rep(1, 5))
  expect_equal(p$data$Angles[1:5], c(0, pi/2, pi, -pi/2, 0))
})

test_that("a bearing of more than one turn is brought onto the compass", {
  p <- directionPlot(c(-90, 400, 720), c(1, 2, 3))
  expect_equal(p$data$Angles[1:3], c(-pi/2, 2*pi*40/360, 0))
  expect_equal(sum(is.na(ggplot2::ggplot_build(p)$data[[1]]$x)), 0)
})

test_that("the mean segment is drawn for directions in the western half", {
  # Its ends are placed on the same scale, so they have to be inside it too.
  built <- ggplot2::ggplot_build(directionPlot(c(270, 350), c(1, 3)))
  mean.layer <- built$data[[length(built$data)]]
  expect_equal(nrow(mean.layer), 1)
  expect_equal(sum(is.na(c(mean.layer$x, mean.layer$xend))), 0)
})
