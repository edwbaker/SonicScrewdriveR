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
#' @return A list with one entry per channel, whatever the class and channel
#'   count of the input. Each entry is itself a list.
#' @export
allChannels <- function(w, FUN, cl=NULL, channel.param="channel",  output.FUN=NULL, ...) {
  #Every input is treated as a number of channels, so that the output has the
  #same structure whatever the class and channel count of the input.
  if (is(w, "Wave")) {
    channels <- if (w@stereo) 2 else 1
  } else if (is(w, "WaveMC")) {
    channels <- w@dim[2]
  } else {
    stop("Expecting a Wave or WaveMC object.")
  }
  if (is.null(cl)) {
    ret <- lapply(1:channels, .doChannel, w=w,  channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
  } else {
    ret <- parallel::parLapply(cl, 1:channels, .doChannel, w=w, channel.param=channel.param, output.FUN=output.FUN, FUN, ...)
  }
  return(ret)
}

#' @importFrom tuneR channel Wave
.doChannel <- function(channel, w, channel.param, output.FUN, FUN, ...) {
  if (is.null(channel.param)) {
    if (is(w, "Wave")) {
      if (channel == 1) {
        w <- channel(w, "left")
      } else if (channel == 2) {
        w <- channel(w, "right")
      }
    } else if (is(w, "WaveMC")) {
      #Extract as a Wave, as single channel functions expect the left slot
      w <- tuneR::Wave(
        w@.Data[,channel],
        samp.rate = w@samp.rate,
        bit = w@bit,
        pcm = w@pcm
      )
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
#' @return A list containing the value calculated for the left channel.
#' @export
channels_se <- function(...) {
  params <- list(...)

  #Each index function in soundecology gives the value for the left channel a
  #different name. allChannels() passes one channel at a time, so the left value is
  #the value for the channel being processed.
  values <- c(
    "left_area",      #bioacoustic_index
    "adi_left",       #acoustic_diversity
    "aei_left",       #acoustic_evenness
    "ndsi_left",      #ndsi
    "AciTotAll_left"  #acoustic_complexity
  )

  found <- values[values %in% names(params)]
  if (length(found) == 0) {
    stop("Not the output of a supported soundecology index function.")
  }
  return(list(params[[found[1]]]))
}
