#' Estimate the direction a sound arrived from
#'
#' Given the time differences of arrival of a sound between the microphones of an
#' array, and the positions of those microphones, estimates the direction the
#' sound arrived from.
#'
#' @details
#' The sound is assumed to be far enough away that the wavefront reaching the
#' array is flat, in which case the delay at each microphone depends on the
#' direction the sound came from but not on how far away it was. "Far enough" is
#' relative to the size of the array: a few array widths is usually sufficient.
#' No distance to the source is estimated, and none can be from a single array
#' under this assumption.
#'
#' The direction is returned as an azimuth measured anticlockwise from the
#' positive x axis of the coordinates the microphone positions were given in. If
#' those coordinates have x pointing east and y pointing north, the compass
#' bearing of the source is `(90 - azimuth) %% 360` degrees.
#'
#' ## Arrays that cannot give a single answer
#' Two microphones, or any number of microphones in a straight line, determine
#' only the angle between the direction of the sound and the axis of the array:
#' every direction on a cone around that axis gives the same delays. For an array
#' lying in a line in two dimensions this leaves two possible bearings, mirrored
#' about the array axis, and both are returned. Microphones lying in a plane,
#' with positions given in three dimensions, likewise leave two possible
#' directions mirrored about that plane. For microphones in a line with positions
#' given in three dimensions no bearing can be recovered, and only the angle from
#' the array axis is returned.
#'
#' ## Checking the result
#' A set of delays that is consistent with a plane wave has a `magnitude` of 1
#' and a `residual` of 0. A magnitude far from 1, or a residual that is an
#' appreciable fraction of the size of the array, means the delays are not
#' consistent with any single direction and the bearing should not be trusted.
#' Common causes are a correlation peak found on the wrong cycle of a tonal
#' sound, a source too close to the array for the plane wave assumption, and two
#' sounds overlapping in the window correlated.
#'
#' Neither check is available from every array. The residual is only meaningful
#' where there are more microphones than one more than the number of dimensions
#' the positions are given in, as fewer leave the delays exactly enough to fit a
#' direction to. The magnitude is only meaningful for an array that determines
#' the direction fully, as one that does not fits only the part of the direction
#' it can see. For a pair of microphones neither is available, and delays that
#' are not physically possible are all that can be detected.
#'
#' ## The speed of sound
#' Where the array determines the direction fully the bearing does not depend on
#' the speed of sound, which only scales the delays: getting it wrong moves the
#' magnitude away from 1 but leaves the direction alone. Where the array leaves
#' an ambiguity the bearing does depend on it, so a pair of microphones is worth
#' giving a speed calculated for the conditions of the recording.
#'
#' @param delays Time differences of arrival in seconds, one per microphone, as
#'   returned by `tdoa()`. Either the data frame `tdoa()` returns, or a numeric
#'   vector in channel order. Channels with a delay of NA are ignored.
#' @param positions Positions of the microphones in metres, as a matrix or data
#'   frame with one row per microphone (in the same order as `delays`) and either
#'   two or three columns.
#' @param speed The speed of sound in metres per second. The default is the value
#'   `soundSpeed()` gives for air, a value for the conditions of a recording can
#'   be obtained with e.g. `soundSpeed(method="cramer", temp=14, RH=80)`.
#' @param unit Unit for the returned angles, either "degrees" (the default) or
#'   "radians".
#' @return A list holding:
#' * **azimuth** The direction the sound arrived from, or two directions where
#'   the array leaves an ambiguity (see Details), or NA where no direction can be
#'   recovered.
#' * **elevation** The angle above the horizontal, for positions given in three
#'   dimensions. NA otherwise.
#' * **direction** A matrix holding the unit vector of each direction in the
#'   coordinates the positions were given in.
#' * **cone** For microphones in a line with positions given in three
#'   dimensions, the angle between the direction of the sound and the axis of the
#'   array, measured from the first microphone towards the last. NA otherwise.
#' * **residual** The root mean square difference, in metres, between the path
#'   length differences the estimate implies and those the delays give.
#' * **magnitude** The length of the direction vector the fit produced, before
#'   it was scaled to a unit vector, which is 1 for delays that are exactly
#'   consistent with a plane wave (see Details). Where the array leaves an
#'   ambiguity this is the length of the part of the direction the array can
#'   see, which is at most 1.
#' * **speed** The speed of sound used.
#' * **unit** The unit the angles are given in.
#' * **ambiguous** Whether the array leaves more than one possible direction.
#' @export
#' @importFrom stats dist
#' @seealso [tdoa()], which produces the delays this function takes.
#' @examples
#' # Three microphones in a right angled triangle, 1m apart
#' mics <- rbind(c(0, 0), c(1, 0), c(0, 1))
#'
#' # A sound arriving from the east reaches the eastern microphone first
#' bearing(c(0, -1/343, 0), mics)
#'
#' # A stereo pair leaves two possible bearings
#' bearing(c(0, -0.5/343), rbind(c(0, 0), c(1, 0)))
#'
bearing <- function(delays, positions, speed=NULL, unit="degrees") {
  if (is.data.frame(delays)) {
    if (!all(c("channel", "delay") %in% names(delays))) {
      stop("A data frame of delays must have channel and delay columns, as returned by tdoa().")
    }
    delays <- delays$delay[order(delays$channel)]
  }
  if (!is.numeric(delays)) {
    stop("delays must be numeric.")
  }
  if (!unit %in% c("degrees", "radians")) {
    stop(paste("Unknown unit for bearing:", unit))
  }
  if (is.null(speed)) {
    speed <- soundSpeed()
  }
  speed <- validateSpeed(speed)

  positions <- as.matrix(positions)
  if (!is.numeric(positions)) {
    stop("positions must be numeric.")
  }
  if (nrow(positions) != length(delays)) {
    stop("positions must have one row for each delay.")
  }
  if (!ncol(positions) %in% c(2, 3)) {
    stop("positions must have two or three columns.")
  }
  #Channels that could not be correlated are dropped rather than allowed to make
  #the whole estimate NA.
  known <- which(!is.na(delays))
  delays <- delays[known]
  positions <- positions[known, , drop=FALSE]
  if (length(delays) < 2) {
    stop("At least two microphones with a known delay are needed.")
  }

  .validateDelaysPossible(delays, positions, speed)

  dimensions <- ncol(positions)
  #Centring both removes the choice of reference microphone from the fit, so
  #that delays measured against any channel give the same answer.
  A <- sweep(positions, 2, colMeans(positions))
  b <- -speed * (delays - mean(delays))

  fit <- .planeWaveFit(A, b)
  return(.bearingAngles(fit, dimensions, speed, unit))
}

#' Least squares fit of a plane wave direction to path length differences
#'
#' @param A Microphone positions, centred on the array.
#' @param b Path length differences implied by the delays, in metres.
#' @return A list holding the direction vectors, the length of the fitted vector,
#'   the residual, the angle from the array axis where the array leaves an
#'   ambiguity, and whether it does.
#' @keywords internal
#' @noRd
.planeWaveFit <- function(A, b) {
  dimensions <- ncol(A)
  #nv is given, because the default leaves out the directions an array of fewer
  #microphones than dimensions cannot see, which are the ones needed to know
  #what it cannot see.
  s <- svd(A, nv=dimensions)
  #An array whose microphones lie in a line (or, in three dimensions, a plane)
  #has fewer singular values than dimensions, and fixes the direction of the
  #sound only within that line or plane.
  rank <- sum(s$d > s$d[1] * 1e-7)
  if (rank == 0) {
    stop("The microphone positions are all the same point.")
  }
  reduced <- seq_len(rank)

  #The least squares direction, within whatever subspace the array spans.
  alpha <- (t(s$u[, reduced, drop=FALSE]) %*% b) / s$d[reduced]
  spanned <- s$v[, reduced, drop=FALSE] %*% alpha
  magnitude <- sqrt(sum(alpha^2))
  residual <- sqrt(mean((A %*% spanned - b)^2))

  if (rank == dimensions) {
    return(list(
      direction = matrix(spanned / magnitude, nrow=1),
      magnitude = magnitude,
      residual = residual,
      cone = NA_real_,
      ambiguous = FALSE
    ))
  }

  unseen <- s$v[, -reduced, drop=FALSE]
  if (ncol(unseen) > 1) {
    #Microphones in a line, with positions given in three dimensions, leave a
    #whole cone of directions rather than a pair of them, and the angle of that
    #cone from the array axis is all there is to report.
    #Measured from the first microphone towards the last, as the axis the
    #singular value decomposition gives could point either way along the array.
    axis <- A[nrow(A),] - A[1,]
    axis <- axis / sqrt(sum(axis^2))
    return(list(
      direction = matrix(NA_real_, nrow=1, ncol=dimensions),
      magnitude = magnitude,
      residual = residual,
      cone = acos(max(-1, min(1, sum(spanned * axis)))),
      ambiguous = TRUE
    ))
  }

  #The part of the direction the array cannot see has whatever length is left
  #over once the part it can see is accounted for. Delays that are not
  #physically possible leave nothing over, and are reported by
  #.validateDelaysPossible() before the fit.
  along <- min(magnitude, 1)
  spanned <- spanned * (along / max(magnitude, .Machine$double.eps))
  perpendicular <- sqrt(1 - along^2)
  return(list(
    direction = rbind(
      as.vector(spanned + perpendicular * unseen),
      as.vector(spanned - perpendicular * unseen)
    ),
    magnitude = magnitude,
    residual = residual,
    cone = NA_real_,
    ambiguous = TRUE
  ))
}

#' Check that delays could have been produced by a sound reaching the array
#'
#' A sound cannot arrive at two microphones further apart in time than it takes
#' sound to travel between them.
#'
#' @param delays Time differences of arrival in seconds.
#' @param positions Microphone positions in metres.
#' @param speed The speed of sound in metres per second.
#' @return The delays unchanged. A warning is raised for delays that are not
#'   physically possible.
#' @keywords internal
#' @noRd
.validateDelaysPossible <- function(delays, positions, speed) {
  separation <- as.matrix(stats::dist(positions))
  travelled <- abs(outer(delays, delays, "-")) * speed
  #A tolerance, so that a delay of exactly the largest possible value is not
  #reported as impossible by a rounding error in its last digit.
  excess <- travelled - separation * (1 + 1e-9)
  if (any(excess > 0)) {
    worst <- which(excess == max(excess), arr.ind=TRUE)[1,]
    warning(paste0(
      "The delay between microphones ", worst[1], " and ", worst[2],
      " is longer than sound takes to travel between them (",
      signif(travelled[worst[1], worst[2]], 3), "m of path difference for a separation of ",
      signif(separation[worst[1], worst[2]], 3), "m). The bearing is unreliable."
    ))
  }
  return(delays)
}

#' Convert fitted direction vectors into angles
#'
#' @param fit The list returned by .planeWaveFit().
#' @param dimensions Number of dimensions the positions were given in.
#' @param speed The speed of sound used.
#' @param unit Unit for the returned angles.
#' @return The list returned by bearing().
#' @keywords internal
#' @noRd
.bearingAngles <- function(fit, dimensions, speed, unit) {
  azimuth <- atan2(fit$direction[,2], fit$direction[,1])
  if (dimensions == 3) {
    elevation <- asin(pmax(-1, pmin(1, fit$direction[,3])))
  } else {
    elevation <- rep(NA_real_, nrow(fit$direction))
  }
  cone <- fit$cone

  if (unit == "degrees") {
    azimuth <- convert2degrees(azimuth)
    elevation <- convert2degrees(elevation)
    cone <- convert2degrees(cone)
  }

  return(list(
    azimuth = azimuth,
    elevation = elevation,
    direction = fit$direction,
    cone = cone,
    residual = fit$residual,
    magnitude = fit$magnitude,
    speed = speed,
    unit = unit,
    ambiguous = fit$ambiguous
  ))
}
