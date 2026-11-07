#' Check a value is one of an allowed set
#'
#' The package validates option arguments in about forty places, each with its own
#' copy of the same guard. This holds the guard once. The default message follows
#' the commonest form in the package, "Unknown <what> <prep> <fn>: <value>", and
#' `msg` supplies the whole message for the call sites that word it differently.
#'
#' @param value The value to check. A vector may be given, in which case every
#'   element must be one of `choices`.
#' @param choices The allowed values.
#' @param what The noun for the argument being checked, e.g. "input" or "method".
#' @param fn The name of the calling function, used in the default message.
#' @param prep The preposition joining `what` to `fn`, either "to" or "for".
#' @param msg A complete error message, used in place of the default one.
#' @param n The number of values the call site takes, or NULL where any number of
#'   them is meaningful. Two valid units are each one of `choices`, so a call site
#'   taking a single unit used to pass them both and fail in the comparison it
#'   reached next.
#' @return The value unchanged. An error is raised if it is not one of `choices`.
#' @keywords internal
#' @noRd
.validateChoice <- function(value, choices, what="input", fn=NULL, prep="to", msg=NULL, n=NULL) {
  if (length(value) == 0) {
    #setdiff() of a zero-length value is empty, so there was nothing to report
    #and NULL was handed on to a caller that could not use it, which then failed
    #with an R error naming neither the argument nor the function.
    bad <- if (is.null(value)) "NULL" else "no value"
  } else if (!is.null(n) && length(value) != n) {
    bad <- paste(value, collapse=", ")
  } else {
    bad <- setdiff(value, choices)
  }
  if (length(bad) == 0) {
    return(value)
  }
  if (is.null(msg)) {
    msg <- paste0("Unknown ", what, " ", prep, " ", fn, ": ", paste(bad, collapse=", "))
  }
  stop(msg)
}
#' Check a numeric value lies within a range
#'
#' The physical-quantity validators were nine copies of "numeric or stop, in range
#' or stop, return". This holds that once. The bounds are tested with `any()`, so
#' a vector is accepted; the copies each tested a length-one condition, which made
#' `convert2Kelvin(c(0, 100))` an error rather than two temperatures.
#'
#' @param x The value to check.
#' @param what The noun for the quantity, used in the default "must be numeric"
#'   message.
#' @param min Lowest permitted value, or NULL for no lower bound.
#' @param max Highest permitted value, or NULL for no upper bound.
#' @param range.msg Message raised when a bound is exceeded.
#' @param type.msg Message raised when the value is not numeric.
#' @return The value unchanged. An error is raised if it is out of range.
#' @keywords internal
#' @noRd
.validateNumericRange <- function(x, what, min=NULL, max=NULL, range.msg=NULL, type.msg=NULL) {
  v <- unlist(x)
  if (!is.numeric(v)) {
    stop(if (is.null(type.msg)) paste(what, "must be numeric") else type.msg)
  }
  #NA is an unknown value, not an out-of-range one. any() on it is NA, so a
  #single missing element used to abort the whole call with R's own "missing
  #value where TRUE/FALSE needed", naming neither the argument nor the
  #function. validateTimeInSeconds() below already passes an NA through.
  known <- v[!is.na(v)]
  if (!is.null(min) && any(known < min)) {
    stop(range.msg)
  }
  if (!is.null(max) && any(known > max)) {
    stop(range.msg)
  }
  return(x)
}

validateRH <- function(RH) {
  .validateNumericRange(
    RH, "RH", min=0, max=100,
    range.msg="Relative humidity must be between 0 and 100."
  )
}

validateBulkModulus <- function(b) {
  .validateNumericRange(b, "Bulk modulus", min=0, range.msg="Bulk modulus must not be negative.")
}

validateSpeed <- function(b) {
  #A speed of sound of zero is as unphysical as a negative one, and the bounds
  #.validateNumericRange() takes are inclusive, so the bound is tested here. A
  #negative speed reverses the path-length differences bearing() fits a
  #direction to, and reports the exact opposite direction as though it were
  #sound; zero gives NaN.
  .validateNumericRange(b, "Speed")
  v <- unlist(b)
  if (any(!is.na(v) & v <= 0)) {
    stop("Speed of sound must be greater than zero.")
  }
  return(b)
}

validateWavelength <- function(b) {
  .validateNumericRange(b, "Wavelength", min=0, range.msg="Wavelength must not be negative.")
}

validateDensity <- function(b) {
  .validateNumericRange(b, "Density", min=0, range.msg="Density must not be negative.")
}

validateKelvin <- function(T) {
  .validateNumericRange(T, "Kelvin", min=0, range.msg="Temperatures must be above 0K.")
}

#' Check a pressure is a pressure something could be at
#'
#' The pressures in this package are absolute rather than gauge pressures, so
#' zero is a perfect vacuum and nothing is below it. The bound is exclusive, and
#' so tested here rather than passed to `.validateNumericRange()`, because the
#' expressions that take a pressure divide by it.
#'
#' @param P The pressure to check.
#' @return The value unchanged. An error is raised if it is not a pressure.
#' @keywords internal
#' @noRd
validatePressure <- function(P) {
  .validateNumericRange(P, "Pressure", type.msg="Pressure must be numeric.")
  v <- unlist(P)
  if (any(!is.na(v) & v <= 0)) {
    stop("Pressure must be greater than zero.")
  }
  return(P)
}

#' Check an object is a Wave object
#'
#' Helper function to test that the input is a Wave object. Will create an error if not.
#'
#' A warning of class `waveFormatMismatch` is raised where the wave's samples do
#' not fit the format it declares: a PCM wave holding samples that are not whole
#' numbers, or samples outside the range its bit depth holds, or a floating
#' point wave holding samples outside -1 to 1. `tuneR::Wave()`
#' declares 16-bit PCM unless told otherwise, so floating point samples given to
#' it without `bit=32, pcm=FALSE` make such a wave, and functions that cut,
#' filter or write it round each sample to a whole number. The check reads 1024
#' samples spread evenly over each channel, so that it takes the same time
#' however long the recording is, and can miss a fault confined to a few
#' samples.
#'
#' @param wave Object to test
#' @importFrom methods is
#' @return The Wave object unchanged. An error is raised if it is not a Wave object.
#' @export
#'
validateIsWave <- function(wave) {
  if (!inherits(wave, "Wave")) {
    stop("Expecting a Wave object")
  }
  .validateWaveFormat(wave)
  return(wave)
}

validateIsWaveLike <- function(wave) {
  if (!inherits(wave, c("Wave", "WaveMC"))) {
    stop("Expecting a Wave or WaveMC object")
  }
  .validateWaveFormat(wave)
  return(wave)
}

validateIsWaveMC <- function(wave) {
  if (!inherits(wave, "WaveMC")) {
    stop("Expecting a WaveMC object")
  }
  .validateWaveFormat(wave)
  return(wave)
}

#' Range of sample values a PCM format holds
#'
#' These are the bounds `tuneR::writeWave()` refuses a wave for exceeding.
#'
#' @param bit The bit depth of the format.
#' @return The lowest and highest sample values, or NULL for a bit depth tuneR
#'   does not write as PCM.
#' @keywords internal
#' @noRd
.pcmRange <- function(bit) {
  if (bit == 8) {
    return(c(0, 255))
  }
  if (bit %in% c(16, 24, 32)) {
    return(c(-2^(bit - 1), 2^(bit - 1) - 1))
  }
  return(NULL)
}

#' Samples spread evenly over a wave, for checking its format
#'
#' The same samples are taken every time, so that a check of them gives the same
#' answer on every call. A wave of `n` samples or fewer is given whole.
#'
#' @param wave A Wave or WaveMC object.
#' @param n The number of samples to take from each channel.
#' @return A vector of the samples taken from every channel.
#' @keywords internal
#' @noRd
.formatSamples <- function(wave, n=1024) {
  mc <- inherits(wave, "WaveMC")
  len <- if (mc) nrow(wave@.Data) else length(wave@left)
  if (len == 0) {
    return(numeric())
  }
  #Spread over more than n samples the steps are longer than one, so no two of
  #them round to the same sample. floor() rounds them, as round() takes about
  #a hundred microseconds over a thousand values, and the validators are called
  #once for every window windowing() cuts.
  i <- if (len <= n) seq_len(len) else floor(1.5 + (seq_len(n) - 1) * ((len - 1) / (n - 1)))
  if (mc) {
    return(as.vector(wave@.Data[i, , drop=FALSE]))
  }
  return(c(wave@left[i], if (wave@stereo) wave@right[i]))
}

#' Warn where a wave's samples do not fit the format it declares
#'
#' tuneR's validity rules check that the bit depth and the pcm flag make a
#' format, but not that the samples fit it. `tuneR::Wave()` declares 16-bit PCM
#' by default, so floating point samples between -1 and 1 given to it make a
#' wave claiming to be integer PCM. Every function that rebuilds a wave rounds
#' its samples to the format it declares, so such a wave is rounded to -1, 0
#' and 1 at the first cut, with nothing to say so.
#'
#' Samples outside the range of the format are warned about too, PCM or
#' floating point. Nothing here clips them, but `tuneR::writeWave()` refuses a
#' wave holding them, and a level read as a proportion of full scale exceeds
#' one.
#'
#' A warning rather than an error, as many existing callers build waves this
#' way. It carries the class `waveFormatMismatch`, so that a caller can handle
#' it alone, and no call, as the validators are reached from helpers whose names
#' would mean nothing to the caller.
#'
#' @param wave A Wave or WaveMC object.
#' @return The wave unchanged, invisibly.
#' @keywords internal
#' @noRd
.validateWaveFormat <- function(wave) {
  x <- .formatSamples(wave)
  x <- x[!is.na(x)]
  #tuneR's validity rules let a 32-bit wave leave its pcm slot NA, which says
  #nothing of the format to check the samples against.
  if (length(x) == 0 || is.na(wave@pcm)) {
    return(invisible(wave))
  }
  if (!wave@pcm) {
    if (min(x) < -1 || max(x) > 1) {
      .waveFormatWarning(
        "The wave declares floating point samples but holds samples outside ",
        "the range -1 to 1 that format holds. tuneR::writeWave() refuses such ",
        "a wave, and tuneR::normalize() scales one into range."
      )
    }
    return(invisible(wave))
  }
  format <- paste0(wave@bit, "-bit PCM")
  if (!is.integer(x) && any(x != trunc(x))) {
    .waveFormatWarning(
      "The wave declares ", format, " but holds samples that are not whole ",
      "numbers, which are rounded wherever the wave is cut, filtered or written. ",
      "Floating point samples need bit=32 (or 64) and pcm=FALSE."
    )
  }
  limits <- .pcmRange(wave@bit)
  if (!is.null(limits) && (min(x) < limits[1] || max(x) > limits[2])) {
    .waveFormatWarning(
      "The wave declares ", format, " but holds samples outside the range ",
      sprintf("%.0f to %.0f", limits[1], limits[2]), " that format holds. ",
      "tuneR::writeWave() refuses such a wave, and tuneR::normalize() scales ",
      "one into range."
    )
  }
  return(invisible(wave))
}

#' Raise a warning that a wave's samples do not fit its format
#'
#' @param ... Parts of the message.
#' @return Nothing, called for the condition it raises.
#' @keywords internal
#' @noRd
.waveFormatWarning <- function(...) {
  warning(structure(
    class = c("waveFormatMismatch", "warning", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' Evaluate an expression without the wave format warning
#'
#' A function that cuts the same wave many times, once per window or per shift,
#' reaches a validator on every cut. It checks the wave once itself and
#' evaluates the cuts here, so that a wave whose samples do not fit its format
#' is warned about once rather than once per cut, and a function handing its
#' wave to another that checks it again does the same. `normalise()` checks its
#' wave here too, as scaling the samples into the format is what it is for,
#' `autoBandPass()` measures here a wave seewave returned in a format its
#' samples do not fit, and `tagApply()` tags here the wave a function gave
#' back, which is warned about where it is next used. Other warnings pass
#' through.
#'
#' @param expr The expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
.withoutFormatWarning <- function(expr) {
  withCallingHandlers(
    expr,
    waveFormatMismatch = function(w) invokeRestart("muffleWarning")
  )
}

validateFreq <- function(f) {
  .validateNumericRange(
    f, "Frequency", min=0,
    range.msg="Frequency must be positive.", type.msg="Frequency must be numeric."
  )
}

#' Check a sample rate is a rate at which something could be sampled
#'
#' A sample rate of zero gives an infinite duration for any number of samples,
#' and a negative one a negative duration, so the bound is exclusive and tested
#' here rather than passed to `.validateNumericRange()`.
#'
#' @param samp.rate The sample rate to check.
#' @return The value unchanged. An error is raised if it is not a possible rate.
#' @keywords internal
#' @noRd
validateSampRate <- function(samp.rate) {
  .validateNumericRange(samp.rate, "Sample rate", type.msg="Sample rate must be numeric.")
  v <- unlist(samp.rate)
  if (any(!is.na(v) & v <= 0)) {
    stop("Sample rate must be greater than zero.")
  }
  return(samp.rate)
}

#' Check a count of something is a positive whole number
#'
#' Bit depths and channel counts are counts of indivisible things, so a
#' fractional or negative one describes no recording that could exist.
#'
#' @param x The value to check.
#' @param what The noun for the quantity, used in the messages.
#' @return The value unchanged. An error is raised if it is not such a count.
#' @keywords internal
#' @noRd
.validatePositiveCount <- function(x, what) {
  v <- unlist(x)
  if (!is.numeric(v)) {
    stop(paste(what, "must be numeric"))
  }
  known <- v[!is.na(v)]
  if (any(known <= 0 | known != round(known))) {
    stop(paste(what, "must be a positive whole number"))
  }
  return(x)
}

validateFreqIsPossible <- function(f, wave=NULL, samp.rate=NULL) {
  validateFreq(f)
  #The wave/samp.rate arguments do not vary over the frequencies, so they are
  #checked once rather than once per frequency. An empty f is now checked too,
  #where the loop over seq_along() used to accept it without a sample rate.
  if (is.null(wave) & is.null(samp.rate)) {
    stop("Frequency requires Wave object or samp.rate")
  }
  if (!is.null(wave) & !is.null(samp.rate)) {
    stop("Frequency requires Wave object OR samp.rate")
  }
  if (!is.null(wave)) {
    validateIsWave(wave)
    samp.rate <- wave@samp.rate
  }
  if (!is.numeric(samp.rate)) {
    stop("samp.rate must be numeric")
  }
  if (any(unlist(f) > samp.rate/2)) {
    stop("Frequency is greater than half sample rate.")
  }
  return(f)
}

validateBandwidthIsPossible <-function(bw, wave=NULL, samp.rate=NULL){
  #Same tests as for frequency
  tryCatch(
    validateFreqIsPossible(bw, wave=wave, samp.rate=samp.rate),
    error = function(e) {
      stop(gsub("Frequency", "Bandwidth", e[1]))
    }
  )
  return(bw)
}

validateQ <- function(Q) {
  .validateNumericRange(
    Q, "Q", min=0,
    range.msg="Q must be positive.", type.msg="Q must be numeric."
  )
}

validateDutyCycle <- function(dc) {
  .validateNumericRange(
    dc, "Duty cycle", min=0,
    range.msg="Duty cycle must be greater than or equal to zero.",
    type.msg="Duty cycle must be numeric."
  )
  .validateNumericRange(
    dc, "Duty cycle", max=1,
    range.msg="Duty cycle must be less than or equal to one."
  )
}

validateSpectrum <- function(s, coerceNegative=FALSE, coerceNA = TRUE) {
  if (typeof(s) != "double") {
    stop("Spectrum must be double.")
  }
  if (!is(s, "matrix")) {
    stop("Spectrum must be a matrix.")
  }
  if (ncol(s) != 2) {
    stop("Spectrum must have two columns.")
  }
  if (nrow(s) < 1) {
    stop("Spectrum must have one or more rows.")
  }
  #is.na() is TRUE of NaN as well as NA, so the rule below for an unknown
  #amplitude took in NaN too and replaced it with zero. NaN is not an unknown
  #value but a calculation that produced no number - dividing by a total or a
  #largest bin of zero, say - and there is nothing for it to stand in for.
  #Coercing it presented a failed calculation to the caller as a measurement,
  #so it is reported however coerceNA is set.
  if (any(is.nan(s))) {
    stop("No NaN values allowed in spectra.")
  }
  na <- is.na(s)
  if (any(na)) {
    if (!coerceNA) {
      stop("No NA allowedin spectra.")
    }
    #An unknown amplitude can stand as zero, but there is no value to stand in
    #for an unknown frequency bin. The element-by-element version reached
    #`if (NA < 0)` on those and aborted with R's own message.
    if (any(na[, 1])) {
      stop("No NA frequencies allowed in spectra.")
    }
    s[, 2][na[, 2]] <- 0
  }
  negative <- s < 0
  if (any(negative)) {
    if (!coerceNegative) {
      stop("No negative values in spectrum.")
    }
    s[negative] <- 0
  }
  return(s)
}

validateComparableSpectra <- function(s1, s2) {
  validateSpectrum(s1)
  validateSpectrum(s2)
  if (nrow(s1) != nrow(s2)) {
    stop("Spectra must have equal number of rows.")
  }
  if (all(s1[,1] == s2[,1]) != TRUE) {
    stop("Spectra must have same frequency bins.")
  }

}

validateTimeInSeconds <- function(t, coerceNegative=FALSE, max_t=NULL, coerceMaximum=FALSE) {
  if (!is.numeric(t)) {
    stop("Time in Seconds must be numeric.")
  }
  #NA is an unknown time, not a negative one. any() on it used to abort with
  #"missing value where TRUE/FALSE needed" rather than passing the NA through.
  negative <- !is.na(t) & t < 0
  if (any(negative)) {
    if (!coerceNegative) {
      stop("Time in Seconds cannot be negative")
    }
    t[negative] <- 0
  }
  if (!is.null(max_t)) {
    over <- !is.na(t) & t > max_t
    if (any(over)) {
      if (!coerceMaximum) {
        stop("Time in Seconds cannot be longer than max_t")
      }
      t[over] <- max_t
    }
  }
  return(t)
}
