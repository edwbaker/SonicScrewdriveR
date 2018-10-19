#' Natural Time Domain
#'
#' Runs a function on the wave and outpiuts values in the Natural Time Domain.
#' 
#' @param wave A Wave object containing pulses
#' @param events Onset of detected events, e.g. from pulseDetction()
#' @param FUN The function to run
#' @param normalise If TRUE the output is a probability density
#' @export
#'

ntd <- function(wave, events, FUN, normalise=FALSE) {
 N <- length(events)-1
 k <- 1:(length(events)-1) / N
 Fk <- vector(length = length(events)-1)
 for (i in 1:(length(events)-1)) {
   wave_event <- tuneR::Wave(wave@left[events[i]:events[i+1]], samp.rate=wave@samp.rate, bit=wave@bit)
   Fk[i] <- do.call(FUN, list(wave=wave_event))
 }
 if (normalise) {
   Fk <- Fk/sum(Fk)
 }
 return(cbind(k,Fk))
}