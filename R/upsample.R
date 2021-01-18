upsample <- function(wave, upsample.rate, method="linear") {
  sf <- upsample.rate / wave@sample.rate
  if (sf != round(sf)) {
    stop("Scale factor is not an integer")
  }

  newleft <- vector(mode="numeric", length=length(wave@left)*sf)
  #TODO: Other channels
  for (i in 1:length(wave@left)) {
      newleft[(sf*i)-1] <- wave@left[i]
  }

  if (method=="linear") {
    newleft <- linear.na(newleft)
  }

  wave@left <- newleft
  wave@samp.rate <- upsample.rate
  return(wave)
}

linear.na <- function(v) {
  #TODO: Interpolate
  return(v)
}
