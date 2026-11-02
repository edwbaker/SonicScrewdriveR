#' @importFrom Rdpack reprompt

.equalWaveTR <- utils::getFromNamespace("equalWave", "tuneR")

#' Tests if two Wave-like objects have the same properties
#'
#' @param w1 a Wave-like object
#' @param w2 a Wave-like object
#' @return a logical value
#' @keywords internal
#' @noRd
.equalWave <- function(w1, w2) {
  return(.equalWaveTR(untagWave(w1), untagWave(w2)))
}

#' Check PCM given bit and pcm
#'
#' @param bit bit depth (see `tuneR`).
#' @param pcm logical value (see `tuneR`).
#' @return a logical value
#' @keywords internal
#' @noRd
.setPCM <- function(bit, pcm) {
  # Modified from tuneR
  if (bit == 1) {
    # Pass through to tuneR
    return(pcm)
  }
  if(bit == 64) {
    if (pcm){
      warning("pcm set to FALSE since bit=64")
      return(FALSE)
    } else {
      return(pcm)
    }
  }
  if(bit %in% c(8, 16, 24)) {
    if (!pcm) {
      warning("pcm set to TRUE since bit was one of 8, 16, or 24")
      return(TRUE)
    } else {
      return(pcm)
    }
  }
  if(bit == 32) {
    return(pcm)
  }
  stop("bit must be one of 8, 16, 24, 32, or 64.")
}

#' Should a single channel function be applied via allChannels()?
#'
#' Single channel functions read the left slot of a Wave. Anything with more
#' than one channel to consider, and any WaveMC (which has no left slot at all),
#' has to go through allChannels() instead.
#' @noRd
.useAllChannels <- function(wave) {
  if (inherits(wave, "WaveMC")) {
    return(TRUE)
  }
  if (inherits(wave, "Wave")) {
    return(wave@stereo)
  }
  return(FALSE)
}

#' Largest value a wave of a given format may hold
#'
#' Eight bit PCM is unsigned and runs to 255, other PCM depths are signed and run
#' to one less than half their range, and a floating point wave holds values
#' between -1 and 1 whatever its bit slot says. Reading the bit slot alone gives
#' 2^32 for the floating point waves that tuneR creates for bit=1.
#'
#' This is the largest sample value, not the largest excursion from silence, and
#' the two differ for eight bit PCM. A level expressed as a proportion wants
#' `.waveFullExcursion()` below.
#'
#' @param w A Wave or WaveMC object.
#' @return The largest positive sample value the format allows.
#' @noRd
.waveFullScale <- function(w) {
  if (w@bit == 8) {
    return(255)
  }
  if (!w@pcm) {
    return(1)
  }
  return(2^(w@bit - 1) - 1)
}

#' Sample value a wave of a given format holds where there is silence
#'
#' Eight bit PCM is unsigned about a midpoint of 128. Every other format the
#' package handles is signed about zero, so this is zero for all of them.
#' normalize() from tuneR keeps an eight bit wave about that midpoint, so the
#' centre is the same before and after normalising.
#'
#' @param w A Wave or WaveMC object.
#' @return The sample value that stands for silence.
#' @noRd
.waveCentre <- function(w) {
  if (w@pcm && w@bit == 8) {
    return(128)
  }
  return(0)
}

#' Largest excursion from silence a wave of a given format may hold
#'
#' `.waveFullScale()` gives the largest sample value, which for eight bit PCM is
#' the whole unsigned span rather than the distance from its midpoint to either
#' end. Reading a level as a proportion of the span rather than of the excursion
#' halves every amplitude and so quarters every power, and compares a threshold
#' against samples that never fall below 128.
#'
#' @param w A Wave or WaveMC object.
#' @return The largest excursion from `.waveCentre()` the format allows.
#' @noRd
.waveFullExcursion <- function(w) {
  if (w@pcm && w@bit == 8) {
    #Eight bit reaches 128 below its midpoint but only 127 above it, so the peak
    #either side is 127: the value generateNoise() scales noise to, and the one
    #.bandPSD() divides a centred eight bit wave by.
    return(127)
  }
  return(.waveFullScale(w))
}

#' Check a wave holds the window seewave's meanspec() takes its spectrum in
#'
#' meanspec() takes windows of 512 samples by default, and on a wave holding
#' fewer it failed with "wrong sign in 'by' argument" from seq(), which names
#' neither the wave nor the window. A short window of windowing() is the usual
#' way to reach it.
#'
#' @param wave A mono Wave object.
#' @param fn The name of the function asking, for the message.
#' @param wl The window length the spectrum is taken in.
#' @return The wave unchanged. An error is raised if it is too short.
#' @noRd
.validateSpectrumLength <- function(wave, fn, wl=512) {
  if (length(wave@left) < wl) {
    stop(
      fn, "() takes its spectrum in windows of ", wl, " samples, and the wave ",
      "holds ", length(wave@left), ". Give it a longer wave or window."
    )
  }
  return(wave)
}

#' Samples of a Wave-like object as a matrix
#'
#' A Wave holds its channels in the left and right slots, a WaveMC in the columns
#' of a matrix. This returns either as a matrix with one column per channel, so
#' that functions comparing channels do not need to know which they were given.
#'
#' @param wave A Wave or WaveMC object.
#' @return A matrix with one column per channel, named if the channels are named.
#' @noRd
.channelMatrix <- function(wave) {
  if (inherits(wave, "WaveMC")) {
    return(wave@.Data)
  }
  if (wave@stereo) {
    return(cbind(wave@left, wave@right))
  }
  return(matrix(wave@left, ncol=1))
}

#' One channel of a Wave-like object as a Wave of one channel
#'
#' A Wave holds its channels in the left and right slots, a WaveMC in the columns
#' of a matrix. This returns one of them as a single channel Wave, so that a
#' function given a channel by number need not know which class it was handed.
#'
#' @param wave A Wave or WaveMC object.
#' @param channel The number of the channel to take.
#' @return A Wave object holding that channel alone.
#' @noRd
.waveChannel <- function(wave, channel) {
  samples <- .channelMatrix(wave)
  if (channel < 1 || channel > ncol(samples)) {
    stop(
      "The recording has ", ncol(samples), " channels, so channel ", channel,
      " is not one of them."
    )
  }
  return(.rebuildWave(samples[, channel], wave))
}

#' A wave of the given samples in the format of another
#'
#' Several functions take samples from a wave, or compute new ones from it, and
#' put them in a wave declaring the format of the one they came from. Each wrote
#' that out for itself, and three of the copies were wrong: `cutws()` and `ntd()`
#' left the pcm flag to tuneR's default, and `upsample()` declared integer PCM
#' for samples that were not whole. tuneR's validity rules check the bit and pcm
#' combination but not whether a PCM wave's samples are whole, so nothing
#' complained until `writeWave()` rounded them with only its generic warning to
#' show for it.
#'
#' Samples are rounded wherever the format is integer PCM, so that a function
#' computing samples need not remember to. Samples taken from a PCM wave are
#' whole already, and integer samples are left as they are, as `round()` would
#' return them as doubles. A floating point format is not rounded, as its
#' samples are the values they say they are.
#'
#' A vector gives a mono Wave. A matrix gives a WaveMC where `wave` is one, and
#' otherwise a Wave of its one or two columns, so that a function taking one
#' channel of a multi-channel wave and one keeping all of them can both use it.
#'
#' @param samples A vector of samples, or a matrix with one column per channel.
#' @param wave The Wave or WaveMC object whose format the result declares.
#' @param samp.rate,bit,pcm The format of the result, by default that of `wave`.
#'   Rounding follows `pcm` rather than the format of `wave`.
#' @return A Wave or WaveMC object holding the samples.
#' @noRd
.rebuildWave <- function(samples, wave, samp.rate=wave@samp.rate, bit=wave@bit, pcm=wave@pcm) {
  if (pcm && !is.integer(samples)) {
    samples <- round(samples)
  }
  if (is.matrix(samples) && inherits(wave, "WaveMC")) {
    return(tuneR::WaveMC(samples, samp.rate=samp.rate, bit=bit, pcm=pcm))
  }
  return(tuneR::Wave(samples, samp.rate=samp.rate, bit=bit, pcm=pcm))
}
