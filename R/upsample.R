#' Upsample a wave
#'
#' Used to upsample a Wave object. The upsampled sample rate must be an natural multiple
#' of the current sample rate.
#'
#' @param wave Wave object to upsample.
#' @param upsample.rate The sample rate to upsample to.
#' @param method "basic" for linear, or a function to interpolate NAs in a vector
#' @export
#' @return A resampled Wave object
#' @importFrom tuneR sine
#' @examples
#' wave <- tuneR::sine(4000, samp.rate=44100)
#' wave2 <- upsample(wave, 88200)
#'
upsample <- function(wave, upsample.rate, method="basic") {
  validateIsWave(wave)
  sf <- upsample.rate / wave@samp.rate
  if (sf != round(sf)) {
    stop("Scale factor is not an integer")
  }

  newleft <- rep.int(NA, length(wave)*sf)
  for (i in 1:length(wave)) {
    newleft[sf*(i-1)+1] <- wave@left[i]
    if (method == "basic") {
      d <- (wave@left[i+1] - wave@left[i]) / sf
      for (j in 1:sf-1) {
        newleft[sf*(i-1)+1+j] <- wave@left[i] + j*d
      }
    }
  }
  wave@left <- newleft

  if (length(wave@right > 0)) {
    newright <- rep.int(NA, length(wave)*sf)
    for (i in 1:length(wave)) {
      newright[sf*(i-1)+1] <- wave@right[i]
      if (method == "basic") {
        d <- (wave@right[i+1] - wave@right[i]) / sf
        for (j in 1:sf-1) {
          newright[sf*(i-1)+1+j] <- wave@right[i] + j*d
        }
      }
    }
    wave@right <- newright
  }
  wave@samp.rate <- upsample.rate
  return(wave)
}


