#' Apply a function to all channels of a Wave or WaveMC object
#'
#' Some functions (e.g. ffilter from seewave) only operate on a single
#' channel at a time. This function applies the function to each channel
#' and returns a list of analyses.
#' @param w A Wave or WaveMC object
#' @param FUN Function to apply to the wave.
#' @param cl Optionally a cluster for parallel calculation.
#' @param channel.param Optional. Name of the channel paramater to FUN.
#' @param ... Optional. Additional paramaters to pass to FUN.
#' @return A list of outputs.
#' @export
allChannels <- function(w, FUN, cl=NULL, channel.param="channel",  ...) {
  if (is(w, "Wave")) {
    if (w@stereo == FALSE) {
      ret <- doChannel(1, w, channel.param=channel.param, FUN, ...)
      return(ret)
    } else {
      if (is.null(cl)) {
        ret <- sapply(1:2, doChannel, w=w,  channel.param=channel.param, FUN, ...)
      } else {
        ret <- parSapply(cl, 1:2, doChannel, w=w, channel.param=channel.param, FUN, ...)
      }
      return(ret)
    }
  } else if (is(w, "WaveMC")) {
    if (is.null(cl)) {
      ret <- sapply(1:w@dim[2], doChannel, w=w,  channel.param=channel.param, FUN, ...)
    } else {
      ret <- parSapply(cl, 1:w@dim[2], doChannel, w=w, channel.param=channel.param, FUN, ...)
    }
    return(ret)
  }
}

doChannel <- function(channel, w, channel.param, FUN, ...) {
  l <- list(FUN, w, ...)
  l[as.character(channel.param)] <- channel
  ret <- eval(as.call(l))
  return(ret)
}
