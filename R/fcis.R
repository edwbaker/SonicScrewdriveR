scaleRGB <- function(vector, no.diff=255) {
  vector <- as.numeric(vector)
  #The range is taken over the values that are present. A single missing value
  #used to make the maximum and the minimum NA, which scaled the whole channel to
  #NA and so blacked it out entirely.
  low <- min(vector, na.rm=TRUE)
  high <- max(vector, na.rm=TRUE)
  if (!is.finite(low) || !is.finite(high) || low == high) {
    return(rep(no.diff, length(vector)))
  }
  vector <- (255 / (high - low)) * (vector - low)
  vector[is.na(vector)] <- 0
  return(as.integer(vector))
}

#' Map three vectors to RGB
#'
#' Maps three vectors of equal length to RGB for use in false-colour index
#' spectrograms
#' @param red The red channel vector
#' @param green The green channel vector
#' @param blue The blue channel vector
#' @return A vector of RGB values
#' @importFrom grDevices rgb
#' @export
map2RGB <- function(red, green, blue) {
  red <- scaleRGB(red)
  blue <- scaleRGB(blue)
  green <- scaleRGB(green)
  return(rgb(red, green, blue, maxColorValue=255))
}

#' Per-frequency-bin acoustic indices
#'
#' The indices available as channels of a false-colour index spectrogram. Each is
#' calculated separately for every frequency bin of a spectrogram, giving one value
#' per bin rather than one value for the whole signal.
#'
#' \describe{
#'   \item{power}{Mean of the squared amplitude.}
#'   \item{ACI}{Acoustic complexity index, the summed absolute difference between
#'     consecutive time frames, divided by the total amplitude in the bin.}
#'   \item{entropy}{Temporal entropy of the amplitude, scaled to between 0 and 1. A
#'     bin whose amplitude is constant over time has an entropy of 1.}
#'   \item{background}{An estimate of the background level, taken as the tenth
#'     percentile of the amplitude over time.}
#'   \item{cover}{The proportion of time frames whose amplitude is more than three
#'     times the background level of the bin.}
#' }
#'
#' @export
#' @return A character vector of index names.
#' @examples
#' fcisIndexNames()
#'
fcisIndexNames <- function() {
  return(names(.fcisIndexFunctions))
}

#' @importFrom stats quantile
.fcisIndexFunctions <- list(
  power = function(amp) {
    rowMeans(amp^2)
  },
  ACI = function(amp) {
    if (ncol(amp) < 2) {
      return(vector(mode="numeric", length=nrow(amp)))
    }
    .fcisFinite(rowSums(abs(t(diff(t(amp))))) / rowSums(amp))
  },
  entropy = function(amp) {
    if (ncol(amp) < 2) {
      return(vector(mode="numeric", length=nrow(amp)))
    }
    p <- amp / rowSums(amp)
    plogp <- p * log(p)
    #A bin that is silent for a frame contributes nothing to the entropy.
    plogp[!is.finite(plogp)] <- 0
    .fcisFinite(-rowSums(plogp) / log(ncol(amp)))
  },
  background = function(amp) {
    .fcisBackground(amp)
  },
  cover = function(amp) {
    rowMeans(amp > 3*.fcisBackground(amp))
  }
)

#' Background level of each frequency bin
#'
#' The tenth percentile of each bin over time, matching stats::quantile() with its
#' default type. Each row is sorted once and the percentile read off, as calling
#' quantile() for every bin is several times slower.
#'
#' Sorting the whole matrix at once, by offsetting each row so that the rows do not
#' overlap, is faster still but cannot be used here: spectrogram amplitudes span many
#' orders of magnitude, and adding the offset loses the quietest bins entirely.
#'
#' @param amp A matrix of amplitudes, one row per frequency bin.
#' @return A numeric vector with one background level per bin.
#' @noRd
.fcisBackground <- function(amp, probs=0.1) {
  n <- ncol(amp)
  if (n == 1) {
    return(as.numeric(amp[, 1]))
  }
  sorted <- apply(amp, 1, sort)
  h <- (n - 1) * probs + 1
  lo <- floor(h)
  if (h == lo) {
    return(sorted[lo, ])
  }
  return(sorted[lo, ] + (h - lo) * (sorted[lo + 1, ] - sorted[lo, ]))
}

#' Replace values that are not finite with zero
#'
#' Bins that are silent throughout a window divide by zero, which would otherwise
#' propagate into the colour mapping.
#'
#' @param x A numeric vector
#' @return The vector with non-finite values replaced by zero.
#' @noRd
.fcisFinite <- function(x) {
  x[!is.finite(x)] <- 0
  return(x)
}

#' Calculate per-frequency-bin indices for one window
#'
#' @param wave A Wave object
#' @param indices Names of the indices to calculate
#' @param wl Window length for the spectrogram
#' @return A named list of numeric vectors, one value per frequency bin.
#' @importFrom seewave spectro
#' @noRd
.fcisWindow <- function(wave, indices, wl) {
  #A spectrogram in decibels contains negative values, which the indices are not
  #defined for, so the linear amplitudes are used.
  spec <- spectro(wave, f=wave@samp.rate, wl=wl, plot=FALSE, dB=NULL, norm=FALSE)
  ret <- lapply(indices, function(i) .fcisIndexFunctions[[i]](spec$amp))
  names(ret) <- indices
  ret$freq <- spec$freq
  return(ret)
}

#' Calculate the columns of a false-colour index spectrogram
#'
#' @param x A Wave object or a filename
#' @param window.length Length of each column in samples, or NULL for one column
#'   covering the whole recording
#' @param indices Names of the indices to calculate
#' @param wl Window length for the spectrogram
#' @param cluster A cluster from the 'parallel' package
#' @return A list with one entry per column.
#' @noRd
.fcisColumns <- function(x, window.length, indices, wl, cluster) {
  if (is.null(window.length)) {
    wave <- if (inherits(x, "Wave")) x else readAudio(x)
    return(list(.fcisWindow(wave, indices, wl)))
  }
  return(windowing(
    x,
    window.length = window.length,
    FUN = function(wave, start, window.length) .fcisWindow(wave, indices, wl),
    window.overlap = 0,
    complete.windows = TRUE,
    cluster = cluster
  ))
}

#' Sample rate of the input to a false-colour index spectrogram
#'
#' @param x A Wave object or a vector of filenames
#' @return The sample rate, or NA if it cannot be determined.
#' @noRd
.fcisSampleRate <- function(x) {
  if (inherits(x, "Wave")) {
    return(x@samp.rate)
  }
  rate <- tryCatch({
    info <- av::av_media_info(x[1])
    as.numeric(info$audio[, "sample_rate"])
  }, error = function(e) NA)
  return(rate)
}

#' False-colour index spectrogram
#'
#' Calculates a false-colour index spectrogram, in which three acoustic indices are
#' calculated for each frequency bin of each time window and mapped to the red, green
#' and blue channels of an image. Long recordings can be summarised in a single image,
#' with different sounds appearing as different colours.
#'
#' Indices are scaled across the whole of the input, so that colours are comparable
#' between the windows of one spectrogram but not between separate spectrograms. The
#' available indices are described in \code{\link{fcisIndexNames}}.
#'
#' @param x A Wave object, or a vector of filenames which are treated as consecutive
#'   parts of one recording.
#' @param window.length Length of each column of the image in samples. If NULL, each
#'   file gives a single column.
#' @param indices The three indices to map to the red, green and blue channels.
#' @param wl Window length used for the spectrograms.
#' @param cluster A cluster from the 'parallel' package for multi-core computation.
#'   This is only worthwhile when x is a filename and the windows are long, as each
#'   worker then reads its own part of the file. Passing a Wave object sends a copy
#'   of it to every worker, which takes longer than the calculation itself.
#' @export
#' @return An object of class "fcis", being a list of the colours as a character
#'   matrix of one entry per frequency bin and window, the matrix of values for each
#'   index, the frequencies of the bins in kHz, the duration of a window in seconds,
#'   and the names of the indices used.
#' @examples
#' \dontrun{
#' f <- fcis(wave, window.length=44100)
#' plot(f)
#'
#' f <- fcis(wave, window.length=44100, indices=c("background", "ACI", "cover"))
#' }
fcis <- function(
  x,
  window.length = NULL,
  indices = c("power", "ACI", "entropy"),
  wl = 256,
  cluster = NULL
) {
  if (length(indices) != 3) {
    stop("Three indices are required, one for each of the red, green and blue channels.")
  }
  .validateChoice(indices, fcisIndexNames(), "index", "fcis", prep="for")

  if (is.character(x) && length(x) > 1) {
    columns <- unlist(
      lapply(x, .fcisColumns, window.length=window.length, indices=indices, wl=wl, cluster=cluster),
      recursive = FALSE
    )
  } else {
    columns <- .fcisColumns(x, window.length, indices, wl, cluster)
  }
  if (length(columns) == 0) {
    stop("No complete windows to analyse.")
  }

  bins <- length(columns[[1]][[indices[1]]])
  values <- lapply(indices, function(i) {
    vapply(columns, function(column) column[[i]], numeric(bins))
  })
  names(values) <- indices

  #Scaling happens here rather than per window, so that a colour means the same
  #thing across the whole spectrogram.
  colours <- map2RGB(values[[1]], values[[2]], values[[3]])
  dim(colours) <- c(bins, length(columns))

  samp.rate <- .fcisSampleRate(x)
  ret <- list(
    colours = colours,
    indices = values,
    freq = columns[[1]]$freq,
    window.seconds = if (is.na(samp.rate) || is.null(window.length)) NA else window.length/samp.rate,
    channels = indices
  )
  class(ret) <- "fcis"
  return(ret)
}

#' Plot a false-colour index spectrogram
#'
#' @param x An object of class "fcis"
#' @param xlab Label for the x axis
#' @param ylab Label for the y axis
#' @param main Title for the plot
#' @param ... Additional parameters passed to plot()
#' @export
#' @return No return value, called for its side effect of drawing a plot.
#' @importFrom graphics rasterImage
#' @importFrom grDevices as.raster
#' @examples
#' \dontrun{
#' plot(fcis(wave, window.length=44100))
#' }
plot.fcis <- function(
  x,
  xlab = NULL,
  ylab = "Frequency (kHz)",
  main = "False-colour index spectrogram",
  ...
) {
  bins <- nrow(x$colours)
  n <- ncol(x$colours)

  if (is.na(x$window.seconds)) {
    xmax <- n
    if (is.null(xlab)) {
      xlab <- "Window"
    }
  } else {
    xmax <- n * x$window.seconds
    if (is.null(xlab)) {
      xlab <- "Time (s)"
    }
  }
  ymax <- max(x$freq)

  plot(NA, xlim=c(0, xmax), ylim=c(0, ymax), xaxs="i", yaxs="i",
       xlab=xlab, ylab=ylab, main=main, ...)
  #Rows are reversed so that low frequencies are drawn at the bottom.
  rasterImage(as.raster(x$colours[bins:1, , drop=FALSE]), 0, 0, xmax, ymax, interpolate=FALSE)
  return(invisible(NULL))
}
