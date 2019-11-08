validateRH <- function(RH) {
  if (!is.numeric(RH)) {
    stop("RH must be numeric")
  }
  if (RH>100 | RH < 0) {
    stop("Realtive humidity must be between 0 and 100.")
  }
  return(RH)
}

validateBulkModulus <- function(b) {
  if (!is.numeric(b)) {
    stop("Bulk modulus must be numeric")
  }
  if (b < 0) {
    stop("Bulk modulus must not be negative.")
  }
  return(b)
}

validateSpeed <- function(b) {
  if (!is.numeric(b)) {
    stop("Speed must be numeric")
  }
  return(b)
}

validateWavelength <- function(b) {
  if (!is.numeric(b)) {
    stop("Wavelength must be numeric")
  }
  if (b < 0) {
    stop("Wavelength must not be negative.")
  }
  return(b)
}

validateDensity<- function(b) {
  if (!is.numeric(b)) {
    stop("Density must be numeric")
  }
  if (b < 0) {
    stop("Density must not be negative.")
  }
  return(b)
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
#' Helper function to test that the input is a Wave object. Will create an error if not.
#'
#' @param wave Object to test
#' @export
#'
validateIsWave <- function(wave) {
  if (typeof(wave) != "S4" | class(wave) != "Wave") {
    stop("Expecting a Wave object")
  }
}

validateFreq <- function(f) {
  for (i in 1:length(f)) {
    if (!is.numeric(f[[i]])) {
      stop("Frequency must be numeric.")
    }
    if (f[[i]] < 0) {
      stop("Frequency must be positive.")
    }
  }
  return(f)
}

validateFreqIsPossible <- function(f, wave=NULL, samp.rate=NULL) {
  for (i in 1:length(f)) {
    validateFreq(f[[i]])
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
      if ( f[[i]] > samp.rate/2) {
        stop("Frequency is greater than half sample rate.")
      }
    }
    if (!is.null(wave)) {
      if (f[[i]] > wave@samp.rate/2) {
        stop("Frequency is greater than half sample rate.")
      }
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

validateSpectrum <- function(s, coerceNegative=FALSE, coerceNA = TRUE) {
  if (typeof(s) != "double") {
    stop("Spectrum must be double")
  }
  if (class(s) != "matrix") {
    stop("Spectrum must be a matrix.")
  }
  if (ncol(s) != 2) {
    stop("Spectrum must have two columns.")
  }
  if (nrow(s) < 1) {
    stop("Spectrum must have one or more rows.")
  }
  for (i in 1:nrow(s)) {
    for (j in 1:2) {
      if (!is.numeric(s[i,j])) {
        stop("All values in sepctrum must be numeric.")
      }
      if (is.na(s[[i,j]])) {
        if (coerceNA) {
          if (j==2) {
            s[[i,j]] <- 0
          }
        } else {
          stop("No NA allowedin spectra.")
        }
      }
      if (s[[i,j]] < 0) {
        if (coerceNegative) {
          s[[i,j]] <- 0
        } else {
          stop("No negative values in spectrum.")
        }
      }
    }
  }
  return(s)
}

validateComparableSpectra <- function(s1, s2) {
  if (nrow(s1) != nrow(s2)) {
    stop("Spectra must have equal number of rows.")
  }
  if (all(s1[,1] == s2[,1]) != TRUE) {
    stop("Spectra must have same frequency bins.")
  }

}

validateTimeInSeconds <- function(t, coerceNegative=FALSE, max_t=NULL, coerceMaximum=FALSE) {
  for (i in 1:length(t)) {
    if (!is.numeric(t[[i]])) {
      stop("Time in Seconds must be numeric.")
    }
    if (t[[i]] < 0) {
      if (coerceNegative) {
        t[[i]] <- 0
      } else {
        stop("Time in Seconds cannot be negative")
      }
    }
  }
  if (!is.null(max_t)){
    if (t[[i]] > max_t) {
      if (coerceMaximum) {
        t[[i]] <- max_t
      } else {
        stop("Time in Seconds cannot be longer than max_t")
      }
    }
  }
  return(t)
}
