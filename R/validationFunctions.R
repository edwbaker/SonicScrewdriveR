validateRH <- function(RH) {
  if (!is.numeric(RH)) {
    stop("RH must be numeric")
  }
  if (RH>100 | RH < 0) {
    stop("Realtive humidity must be between 0 and 100.")
  }
  return(RH)
}

validateKelvin <- function(T) {
  if (!is.numeric(T)) {
    stop("Kelvin must be numeric")
  }
  if (T < 0) {
    stop("Temperatures must be above 0K.")
  }
  return(T)
}

validateIsWave <- function(wave) {
  if (typeof(wave) != "S4" | class(wave) != "Wave") {
    stop("Expecting a Wave object")
  }
}

validateFreqIsPossible <- function(f, wave=NULL, samp.rate=NULL) {
  if (!is.numeric(f)) {
    stop("Frequency must be numeric.")
  }
  if (is.null(wave) & is.null(samp.rate)) {
    stop("Validation of frequency requires Wave object or samp.rate")
  }
  if (!is.null(wave) & !is.null(samp.rate)) {
    stop("Validation of frequency requires Wave object OR samp.rate")
  }
  if (!is.null(wave)) {
    validateIsWave(wave)
  }
  if (!is.null(samp.rate) & !is.numeric(samp.rate)) {
    stop("samp.rate must be numeric")
  }
  if (!is.null(samp.rate)) {
    if ( f > samp.rate/2) {
      stop("Frequency is greater than half sample rate.")
    }
  }
  if (!is.null(wave)) {
    if (f > wave@samp.rate/2) {
      stop("Frequency is greater than half sample rate.")
    }
  }

  return(f)
}