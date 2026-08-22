#' Delays a plane wave from a given direction would produce at an array.
#'
#' @param positions Microphone positions.
#' @param azimuth Direction the sound came from, anticlockwise from the x axis.
#' @param elevation Angle of the sound above the horizontal.
#' @param speed The speed of sound.
#' @return Delays in seconds, relative to the first microphone.
#' @noRd
planeWaveDelays <- function(positions, azimuth, elevation=0, speed=343) {
  azimuth <- azimuth * pi/180
  elevation <- elevation * pi/180
  direction <- c(
    cos(azimuth) * cos(elevation),
    sin(azimuth) * cos(elevation),
    sin(elevation)
  )[seq_len(ncol(positions))]
  delays <- -as.vector(positions %*% direction) / speed
  return(delays - delays[1])
}

triangle <- rbind(c(0, 0), c(1, 0), c(0, 1))
tetrahedron <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))

test_that("bearing recovers the direction a sound came from", {
  for (azimuth in c(0, 30, 90, 175, -120)) {
    b <- bearing(planeWaveDelays(triangle, azimuth), triangle)
    expect_equal(b$azimuth, azimuth)
    expect_false(b$ambiguous)
    expect_equal(b$magnitude, 1)
    expect_equal(b$residual, 0)
    expect_true(is.na(b$elevation))
    expect_true(is.na(b$cone))
    expect_equal(b$speed, soundSpeed())
    expect_equal(b$unit, "degrees")
  }
})

test_that("bearing recovers elevation from an array in three dimensions", {
  b <- bearing(planeWaveDelays(tetrahedron, 40, 20), tetrahedron)
  expect_equal(b$azimuth, 40)
  expect_equal(b$elevation, 20)
  expect_false(b$ambiguous)
  expect_equal(b$magnitude, 1)
  expect_equal(dim(b$direction), c(1, 3))
})

test_that("bearing returns angles in radians when asked", {
  b <- bearing(planeWaveDelays(triangle, 30), triangle, unit="radians")
  expect_equal(b$azimuth, 30 * pi/180)
  expect_equal(b$unit, "radians")
})

test_that("bearing uses the speed of sound it is given", {
  delays <- planeWaveDelays(triangle, 30, speed=343)
  expect_equal(bearing(delays, triangle, speed=343)$azimuth, 30)
  expect_equal(bearing(delays, triangle, speed=330)$speed, 330)

  #An array that determines the direction fully gives the same bearing whatever
  #speed of sound it is given, as the speed only scales every delay. A wrong
  #speed shows up in the magnitude instead.
  expect_equal(bearing(delays, triangle, speed=330)$azimuth, 30)
  expect_equal(bearing(delays, triangle, speed=330)$magnitude, 330/343)

  #An array that does not gives a different bearing.
  pair <- rbind(c(0, 0), c(1, 0))
  expect_equal(bearing(planeWaveDelays(pair, 60), pair, speed=343)$azimuth, c(60, -60))
  expect_false(isTRUE(all.equal(
    bearing(planeWaveDelays(pair, 60), pair, speed=330)$azimuth,
    c(60, -60)
  )))

  cold <- soundSpeed(method="cramer", temp=0)
  expect_equal(
    bearing(planeWaveDelays(triangle, 30, speed=cold), triangle, speed=cold)$azimuth,
    30
  )
})

test_that("bearing reports both directions a pair of microphones allows", {
  pair <- rbind(c(0, 0), c(1, 0))
  b <- bearing(planeWaveDelays(pair, 60), pair)
  expect_true(b$ambiguous)
  expect_equal(sort(b$azimuth), c(-60, 60))
  expect_equal(dim(b$direction), c(2, 2))
  #Only the component along the axis of the pair is fitted, the rest of the
  #direction is whatever is left over.
  expect_equal(b$magnitude, cos(60 * pi/180))
  #Both directions are the same distance from the axis of the pair.
  expect_equal(b$direction[1,1], b$direction[2,1])
  expect_equal(b$direction[1,2], -b$direction[2,2])
})

test_that("bearing reports both directions an array in a plane allows", {
  b <- bearing(planeWaveDelays(tetrahedron[1:3,], 40, 20), tetrahedron[1:3,])
  expect_true(b$ambiguous)
  expect_equal(b$azimuth, c(40, 40))
  expect_equal(sort(b$elevation), c(-20, 20))
})

test_that("bearing reports the angle from the axis of a line of microphones", {
  pair <- rbind(c(0, 0, 0), c(1, 0, 0))
  b <- bearing(c(0, -0.5/343), pair)
  expect_true(b$ambiguous)
  expect_equal(b$cone, 60)
  expect_true(is.na(b$azimuth))
  expect_true(is.na(b$elevation))

  #A sound arriving along the axis, from the far microphone towards the first.
  expect_equal(bearing(c(0, 1/343), pair)$cone, 180)
})

test_that("bearing takes the delays tdoa() returns", {
  delays <- planeWaveDelays(triangle, 30)
  d <- data.frame(
    channel = 1:3,
    name = NA_character_,
    delay = delays,
    samples = delays * 48000,
    r = 1
  )
  expect_equal(bearing(d, triangle)$azimuth, 30)
  #The channels are put in order, whatever order the rows are in.
  expect_equal(bearing(d[c(3,1,2),], triangle)$azimuth, 30)
})

test_that("bearing ignores channels with an unknown delay", {
  delays <- planeWaveDelays(tetrahedron, 40, 20)
  delays[4] <- NA
  b <- bearing(delays, tetrahedron)
  #Three microphones in a plane leave two possible directions.
  expect_true(b$ambiguous)
  expect_equal(b$azimuth, c(40, 40))
})

test_that("bearing reports delays that are not physically possible", {
  pair <- rbind(c(0, 0), c(1, 0))
  expect_warning(
    bearing(c(0, -5/343), pair),
    "longer than sound takes to travel between them"
  )
  #The direction is still on the axis of the pair, as that is as close as any
  #direction can get.
  b <- suppressWarnings(bearing(c(0, -5/343), pair))
  expect_equal(b$azimuth, c(0, 0))
  expect_gt(b$magnitude, 1)
})

test_that("bearing reports a fit the delays do not support", {
  #Three microphones in two dimensions leave the delays exactly enough to fit a
  #direction to, so a fourth is needed for there to be anything to disagree.
  square <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1))
  expect_equal(bearing(planeWaveDelays(square, 30), square)$residual, 0)

  #Delays from two different directions, which no single direction explains,
  #but which are each possible for the microphone they belong to.
  delays <- planeWaveDelays(square, 30)
  delays[4] <- planeWaveDelays(square, 120)[4]
  b <- bearing(delays, square)
  expect_gt(b$residual, 0.1)
  expect_lt(b$magnitude, 0.5)
})

test_that("bearing rejects what it cannot use", {
  expect_error(bearing("a", triangle), "delays must be numeric")
  expect_error(bearing(data.frame(a=1), triangle), "channel and delay columns")
  expect_error(bearing(c(0, 0, 0), triangle, unit="grads"), "Unknown unit for bearing")
  expect_error(bearing(c(0, 0), triangle), "one row for each delay")
  expect_error(bearing(c(0, 0, 0), cbind(triangle, 0, 0)), "two or three columns")
  expect_error(bearing(c(0, 0, 0), matrix("a", nrow=3, ncol=2)), "positions must be numeric")
  expect_error(bearing(c(0, NA, NA), triangle), "At least two microphones")
  expect_error(
    bearing(c(0, 0, 0), matrix(0, nrow=3, ncol=2)),
    "positions are all the same point"
  )
})
