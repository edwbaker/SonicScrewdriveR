# The conditions this package raises with a class of its own.
#
# A warning raised while reading audio says that the file is not what it
# claims to be, and a caller commonly wants to handle one of those without
# silencing the rest: a corpus known to hold truncated recordings can pass over
# that one warning and still hear about a sample rate that disagrees with the
# recorder's own metadata. A warning identified only by its wording cannot be
# told from another except by matching text that is free to change, so each
# carries a class.
#
# Every one of them is also classed "audioFileWarning", so that all of them can
# be handled together, and "warning", so that suppressWarnings(), options(warn)
# and a plain warning handler treat it as the warning it is.
#
# The classes raised here are:
#
#   sampleRateMismatch  The sample rate in the header disagrees with the one the
#                       recorder wrote into its own metadata.
#   unwrittenAudioSize  The size of the audio was never written, so the
#                       recording was read to the end of the file.
#   truncatedAudio      The audio runs past the end of the file, which is
#                       shorter than it declares.
#   nonFiniteSamples    Samples that are not finite numbers were read as zero.
#   badDs64             The ds64 chunk of an RF64 or BW64 file cannot be
#                       believed, so its audio may not have been read in full.
#   frameCountMismatch  The ds64 chunk of an RF64 or BW64 file gives a number
#                       of sample frames that its audio does not hold.
#
# The errors with classes of their own are raised in wavChunks.R, where the
# chunk reading that detects them lives: `unsupportedWav` by
# `.unsupportedWav()`, and `badDs64`, a kind of `unsupportedWav`, by
# `.badDs64()`. A file refused there for its ds64 chunk is warned about with
# that same class where its audio is read all the same.

#' Raise a warning about the file audio was read from
#'
#' The call is left out of the condition: these warnings are raised from the
#' helpers that detect each fault rather than from the function the caller
#' called, so reporting the call would name an internal helper and say nothing
#' useful about where the file was read.
#'
#' @param class The class of this warning, which names the fault it reports
#' @param ... Parts of the message
#' @return Nothing, called for the condition it raises
#' @keywords internal
#' @noRd
.audioFileWarning <- function(class, ...) {
  warning(structure(
    class = c(class, "audioFileWarning", "warning", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}
