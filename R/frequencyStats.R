#' Various measurements of frequency values for a Wave object
#'
#' Calculates the peak, centre, bandwidth and quality factor. The quality factor (Q) is calculated at both
#' -3dB and -10dB as discussed by Bennett-Clark (1999)  <doi: 10.1080/09524622.1999.9753408>.
#' 
#' @param wave A Wave object
#' @param plot IF TRUE displays values
#' @param warn If TRUE provides warnings when values are not consistent
#' @importFrom graphics abline plot
#' @export
#' 
frequencyStats <- function(
  wave,
  warn = TRUE,
  plot=FALSE
) {
  if (!typeof(wave) == "S4" | !class(wave) == "Wave") {
    stop("frequencyStats expects a Wave object")
  }
  wave_spec <- seewave::meanspec(wave, norm=FALSE, plot=FALSE)
  
  x <- wave_spec[,1]
  y <- wave_spec[,2]
  
  y<- y^2
  a <- y >= 0.5*max(y)
  b <- y >= 0.1*max(y)
  r <- rle(a)
  s <- rle(b)
  l <- c(0,cumsum(r$lengths))
  m <- c(0,cumsum(s$lengths))
  
  regions <- r$lengths[which(r$values == TRUE)]
  regions_l <- l[which(r$values == TRUE)]
  
  regions_s <- s$lengths[which(s$values == TRUE)]
  regions_l_s <- m[which(s$values == TRUE)]
  
  longest <- which(regions == max(regions))
  longest_s <- which(regions_s == max(regions_s))
  
  min_3 <- x[regions_l[longest]]
  max_3 <- x[regions_l[longest] + regions[longest]]
  
  min_10 <- x[regions_l_s[longest_s]]
  max_10 <- x[regions_l_s[longest_s] + regions_s[longest_s]]
  
  if(warn) {
    if (min_3 >= max_3) {
      warning("-3dB: calculated max greater than min")
    }
    if (min_10 >= max_10) {
      warning("-10dB: calculated max greater than min")
    }
  }
  
  data <- list(
    "-3dB" = list(
      "min" = validateFreqIsPossible(min_3, samp.rate=wave@samp.rate),
      "max" = validateFreqIsPossible(max_3, samp.rate=wave@samp.rate),
      "peak" = validateFreqIsPossible(x[which(y==max(y))], samp.rate=wave@samp.rate),
      "centre" = validateFreqIsPossible(mean(c(min_3,max_3)), samp.rate=wave@samp.rate),
      "bandwidth" = validateBandwidthIsPossible(max_3 - min_3, samp.rate=wave@samp.rate),
      "Q" = validateQ(x[which(y==max(y))] / (max_3 - min_3))
    ),
    "-10dB" = list(
      "min" = validateFreqIsPossible(min_10, samp.rate=wave@samp.rate),
      "max" = validateFreqIsPossible(max_10, samp.rate=wave@samp.rate),
      "peak" = validateFreqIsPossible(x[which(y==max(y))], samp.rate=wave@samp.rate),
      "centre" = validateFreqIsPossible(mean(c(min_10,max_10)), samp.rate=wave@samp.rate),
      "bandwidth" = validateBandwidthIsPossible(max_10 - min_10, samp.rate=wave@samp.rate),
      "Q" = validateQ(x[which(y==max(y))] / (max_10 - min_10))
    )
  )
  
  if(plot) {
    plot(x,y, type="l")
    abline(h=0.5*max(y), v=c(min_3,max_3), col="blue")
    abline(h=0.1*max(y), v=c(min_10,max_10), col="green")
  }
  
  return(data)
}