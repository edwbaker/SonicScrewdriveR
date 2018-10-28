#' Frequency Filtering
#'
#' Allows for complex frequency filtering of Wave objects.
#' 
#' @param wave A Wave object
#' @param FUN A function to provide the filter value at a frequency
#' @param highpass If numeric cut off frequewncies below this cut off
#' @param ... Additional parameters to provide to the filter function
#' 
#' @importFrom stats dnorm fft
#' @export
#' 
freqfilter <- function(
  wave,
  FUN=NULL,
  highpass=NULL,
  ...
) {
  ft <- fft(wave@left)
  n <- length(wave@left)
  f <- wave@samp.rate
  x <- (as.numeric((0:(n - 1))) * f/n/1000)[1:(n%/%2)]
  
  r <- Re(ft[1:length(x)])
  p <- which(r == max(r, na.rm=TRUE))
  if (!is.null(FUN)) {
    mean=x[p]
    filt <- lapply(x, FUN, mean=x[p], ...)
    fs <- cbind(x,ft,filt,as.complex(ft)*as.complex(filt))
    wave@left <- Re(fft(as.complex(fs[,4]), inverse=T))
  }
  if (!is.null(highpass)) {
    filt <- x
    filt[filt < highpass] <- 0
    filt[filt!=0] <- 1
    fs <- cbind(x,ft,filt,as.complex(ft)*as.complex(filt))
    wave@left <- Re(fft(as.complex(fs[,4]), inverse=T))
  }
  return(wave)
}

#' Autoamtic band pass
#'
#' Allows for automatic complex frequency filtering of Wave objects, using a normal distribution.
#' 
#' @param wave A Wave object
#' @param sd Standard deviation of filter
#' @param nsd Number of stanbard deviations eahc side of the mean to pass
#' @param ... Additional parameters to provide to the filter function
#' @importFrom stats dnorm fft
#' @export
#'
autoBandPass <- function(wave, sd, nsd) {
  wave <- freqfilter(wave, normalFilter, sd=1, nsd=1)
}

normalFilter <- function(i, mean, sd, nsd) {
  if (is.na(i)) {
    return(0)
  }
  if (!is.numeric(i)) {
    return(0)
  }
  if (i < (mean - sd * nsd) | i > (mean + sd * nsd)) {
    return(0)
  } else { 
    return(dnorm(i,mean,sd)/dnorm(mean,mean,sd))
  }
}
