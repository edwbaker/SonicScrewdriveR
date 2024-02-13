#' Apply a function to all channels of a Wave or WaveMC object
#'
#' Some functions (e.g. ffilter from seewave) only operate on a single
#' channel at a time. This function applies the function to each channel
#' and returns a list of analyses.
#' @param w A Wave or WaveMC object
#' @param FUN Function to apply to the wave.
#' @param cl Optionally a cluster for parallel calculation.
#' @param channel.param Name of the channel parameter to FUN. Can be NULL.
#' @param output.FUN Optional. Function that processes the output of FUN.
#'   The "channels_se" function provides standard functionality for the
#'   soundecology package.
#' @param ... Optional. Additional parameters to pass to FUN.
#' @return A list of outputs.
#' @export
allChannels <- function(w, FUN, cl=NULL, channel.param="channel",  output.FUN=NULL, ...) {
  if (is(w, "Wave")) {
    if (w@stereo == FALSE) {
      ret <- .doChannel(1, w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
      return(ret)
    } else {
      if (is.null(cl)) {
        ret <- lapply(1:2, .doChannel, w=w,  channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
      } else {
        ret <- parallel::parLapply(cl, 1:2, .doChannel, w=w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
      }
      return(ret)
    }
  } else if (is(w, "WaveMC")) {
    if (is.null(cl)) {
      ret <- lapply(1:w@dim[2], .doChannel, w=w,  channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
    } else {
      ret <- parallel::parLapply(cl, 1:w@dim[2], .doChannel, w=w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
    }
    return(ret)
  }
}

#' @importFrom tuneR channel
.doChannel <- function(channel, w, channel.param, output.FUN, FUN, ...) {
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

  #Handle when ret is not a list
  if (typeof(ret) != "list") {
    l <- list()
    l[[1]] <- ret
    ret <- l
  }


  if (!is.null(output.FUN)) {
    ret <- do.call(output.FUN, ret)
  }
  return(ret)
}

#' Channels for sound ecology
#'
#' Used to process the output of acoustic index functions from the soundecology
#' package when using allChannels.
#'
#' @param ... Export from a bioacoustic index function from the soundecology package
#' @export
channels_se <- function(...) {
  params = list(...)
  if ("left_area" %in% names(params)) {
    return(list(params$left_area))
  }
  if ("adi_left" %in% names(params)) {
    return(list(params$adi_left))
  }
  if ("aei_left" %in% names(params)) {
    return(list(params$aei_left))
  }
}
