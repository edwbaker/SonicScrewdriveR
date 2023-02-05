#' Apply a function to all channels of a Wave or WaveMC object
#'
#' Some functions (e.g. ffilter from seewave) only operate on a single
#' channel at a time. This function applies the function to each channel
#' and returns a list of analyses.
#' @param w A Wave or WaveMC object
#' @param FUN Function to apply to the wave.
#' @param cl Optionally a cluster for parallel calculation.
#' @param channel.param Optional. Name of the channel parameter to FUN.
#' @param output.FUN Optional. Function that processes the output of FUN.
#'   The "channels_se" function provides standard functionality for the
#'   soundecology package.
#' @param ... Optional. Additional paramaters to pass to FUN.
#' @return A list of outputs.
#' @export
allChannels <- function(w, FUN, cl=NULL, channel.param="channel",  output.FUN=NULL, ...) {
  if (is(w, "Wave")) {
    if (w@stereo == FALSE) {
      ret <- doChannel(1, w, channel.param=channel.param, FUN, ...)
      return(ret)
    } else {
      if (is.null(cl)) {
        ret <- sapply(1:2, doChannel, w=w,  channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
      } else {
        ret <- parSapply(cl, 1:2, doChannel, w=w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
      }
      return(ret)
    }
  } else if (is(w, "WaveMC")) {
    if (is.null(cl)) {
      ret <- sapply(1:w@dim[2], doChannel, w=w,  channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
    } else {
      ret <- parSapply(cl, 1:w@dim[2], doChannel, w=w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
    }
    return(ret)
  }
}

#' @importFrom tuneR channel
doChannel <- function(channel, w, channel.param, output.FUN, FUN, ...) {
  if (is.null(channel.param)) {
    if (is(w, "Wave")) {
      if (channel == 1) {
        w <- channel(w, "left")
      } else if (channel == 2) {
        w <- channel(w, "right")
      }
    } else if (is(w, "WaveMC")) {
      w <- w[,channel]
    }
  }
  l <- list(FUN, w, ...)
  if (!is.null(channel.param)) {
    l[as.character(channel.param)] <- channel
  }
  ret <- eval(as.call(l))

  if (!is.null(output.FUN)) {
    ret <- do.call(output.FUN, ret)
  }
  return(ret)
}

#' @export
channels_se <- function(left_area, ...) {
  return(left_area)
}
