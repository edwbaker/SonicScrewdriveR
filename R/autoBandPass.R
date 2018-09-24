#' Frequency Filtering
#'
#' Allows for complex frequency filtering of Wave objects.
#' 
#' @param wave A Wave object
#' @param FUN A function to provide the filter value at a frequency
#' @param ... Additional parameters to provide to the filter function
#' 
#' @keywords TDSC
#' @references Stammers (2011) “Audio Event Classification for Urban Soundscape Analysis”. PhD thesis. University of York.
#' @importFrom stats dnorm fft
#' @export
#' @examples
#' library(tuneR)
#' wave <- readWave(system.file("extdata", "1.wav", package="tdsc"))
#' t <- tdsc(wave)
#' emptyBands(t,t)
#' 
freqfilter <- function(
  wave,
  FUN=NULL,
  ...
) {
  ft <- fft(wave@left)
  n <- length(wave@left)
  f <- wave@samp.rate
  x <- (as.numeric((0:(n - 1))) * f/n/1000)[1:(n%/%2)]
  
  r <- Re(ft)
  p <- which(r == max(r))
  
  if (!is.null(FUN)) {

    filt <- lapply(x, FUN, mean=x[p], ...)
    fs <- cbind(x,ft,filt,as.complex(ft)*as.complex(filt))
    wave@left <- Re(fft(as.complex(fs[,4]), inverse=T))
  }
  return(wave)
}

#' Frequency Filtering
#'
#' Allows for complex frequency filtering of Wave objects.
#' 
#' @param wave A Wave object
#' @param FUN A function to provide the filter value at a frequency
#' @param ... Additional parameters to provide to the filter function
#' 
#' @keywords TDSC
#' @references Stammers (2011) “Audio Event Classification for Urban Soundscape Analysis”. PhD thesis. University of York.
#' @importFrom stats dnorm fft
#' @export
#' @examples
#' library(tuneR)
#' wave <- readWave(system.file("extdata", "1.wav", package="tdsc"))
#' t <- tdsc(wave)
#' emptyBands(t,t)
#'
autoBandPass <- function(wave, sd, nsd) {
  wave <- freqfilter(wave, normalFilter, sd=1, nsd=1)
}

normalFilter <- function(i, mean, sd, nsd) {
  if (is.na(i)) {
    return(0)
  }
  if (i < (mean - sd * nsd) | i > (mean + sd * nsd)) {
    return(0)
  } else { 
    return(dnorm(i,mean,sd)/dnorm(mean,mean,sd))
  }
}

#autoBandPass(wave, sd=1, nsd=1) -> w2
#spec(w2)