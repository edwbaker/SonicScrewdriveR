#' Natural Time Domain
#'
#' Runs a function on the wave and outputs values in the Natural Time Domain (see Varotsos, Sarlis & Skordas(2011) <doi:10.1007/978-3-642-16449-1>).
#'
#' @param wave A Wave object containing pulses
#' @param events Onset of detected events, e.g. from pulseDetection()
#' @param FUN The function to run
#' @param ... Additional arguments to FUN
#' @param normalise If TRUE the output is a probability density
#' @param argument If "wave" supplies a weave object to the function, if "vector" supplies the left channel as a numeric vector.
#' @export
#' @return A list of outputs form the applied function
#'

ntd <- function(wave, events, FUN, normalise=FALSE, argument="wave",...) {
 N <- length(events)-1
 k <- 1:(length(events)-1) / N
 Fk <- vector(length = length(events)-1)
 for (i in 1:(length(events)-1)) {
   wave_event <- tuneR::Wave(wave@left[events[i]:events[i+1]], samp.rate=wave@samp.rate, bit=wave@bit)
   if (argument=="wave") {
     Fk[i] <- do.call(FUN, c(list(wave=wave_event),...))
   }
   if (argument=="vector") {
     Fk[i] <- do.call(FUN, c(list(wave_event@left),...))
   }
 }
 if (normalise) {
   Fk <- Fk/sum(Fk)
 }
 return(cbind(k,Fk))
}
