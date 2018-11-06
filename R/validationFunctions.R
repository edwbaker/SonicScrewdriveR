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

#' Check an object is a Wave object
#'
#' Helper function to test that the input is a Wave object. Will crete an error if not.
#' 
#' @param wave Object to test
#' @export
#'
validateIsWave <- function(wave) {
  if (typeof(wave) != "S4" | class(wave) != "Wave") {
    stop("Expecting a Wave object")
  }
}

validateFreqIsPossible <- function(f, wave=NULL, samp.rate=NULL) {
  if (!is.numeric(f)) {
    stop("Frequency must be numeric.")
  }
  if (f < 0) {
    stop("Frequency must be positive.")
  }
  if (is.null(wave) & is.null(samp.rate)) {
    stop("Frequency requires Wave object or samp.rate")
  }
  if (!is.null(wave) & !is.null(samp.rate)) {
    stop("Frequency requires Wave object OR samp.rate")
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
  if (!is.numeric(Q)) {
    stop("Q must be numeric.")
  }
  if (Q < 0){
    stop("Q must be positive.")
  }
  return(Q)
}

validateDutyCycle <- function(dc) {
  if (!is.numeric(dc)) {
    stop("Duty cycle must be numeric.")
  }
  if (dc < 0) {
    stop("Duty cycle must be greater than or equal to zero.")
  }
  if (dc > 1) {
    stop("Duty cycle must be less than or equal to one.")
  }
  return(dc)
}