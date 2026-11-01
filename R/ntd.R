#' Natural Time Domain
#'
#' Runs a function on the wave and outputs values in the Natural Time Domain (see Varotsos, Sarlis & Skordas(2011) <doi:10.1007/978-3-642-16449-1>).
#'
#' @param wave A Wave object containing pulses
#' @param events Onset of detected events, e.g. from pulseDetection(). The
#'   events must be in increasing order. Each interval runs from one event up to
#'   the sample before the next, so the final event bounds the last interval
#'   without belonging to it.
#' @param FUN The function to run
#' @param ... Additional arguments to FUN
#' @param normalise If TRUE the output is a probability density
#' @param argument If "wave" supplies a weave object to the function, if "vector" supplies the left channel as a numeric vector.
#' @export
#' @return A list of outputs form the applied function
#'

ntd <- function(wave, events, FUN, normalise=FALSE, argument="wave",...) {
 #Each interval runs between one event and the next, so fewer than two events
 #gives none. The count used to run backwards, indexing events with NA.
 if (length(events) < 2) {
   stop("ntd requires at least two events.")
 }
 #The intervals are cut with `:`, which counts downwards when its second value
 #is the smaller. Events out of order gave an interval of the samples between
 #them, reversed, and two events at the same sample a spurious one-sample
 #interval; neither is an interval of a pulse train.
 if (any(diff(events) <= 0)) {
   stop("ntd requires events in increasing order.")
 }
 N <- length(events)-1
 k <- seq_len(N) / N
 Fk <- vector(length = N)
 for (i in seq_len(N)) {
   #Each interval runs from one event up to the sample before the next, the
   #half-open convention windowing() cuts its windows with. Both ends used to be
   #included, so an interval of n samples was cut as n+1 and every boundary
   #sample was measured in the interval before it as well as the one after.
   #
   #pcm is carried over with bit, by .rebuildWave(). Left to tuneR's default of
   #TRUE, a float wave reached FUN declaring itself integer PCM, so anything
   #reading the format worked from a full scale 2^31 times too large, and a 64
   #bit float wave failed tuneR's validity rule outright.
   wave_event <- .rebuildWave(wave@left[events[i]:(events[i+1]-1)], wave)
   if (argument=="wave") {
     Fk[i] <- do.call(FUN, c(list(wave=wave_event),...))
   }
   if (argument=="vector") {
     Fk[i] <- do.call(FUN, c(list(wave_event@left),...))
   }
 }
 if (normalise) {
   #There is no density to normalise to where the outputs sum to zero, which is
   #what digital silence, or any FUN returning zero for every interval, gives.
   #Dividing by it gave NaN throughout, and Inf where outputs of opposite sign
   #cancelled, in place of the probability density this argument documents. The
   #outputs are returned as they came back from FUN instead, and the caller is
   #told. An NA total is left to divide as before: a missing output is a
   #different matter from a total of nothing.
   total <- sum(Fk)
   if (!is.na(total) && total == 0) {
     warning("ntd outputs sum to zero, so they cannot be normalised to a probability density and are returned unnormalised.")
   } else {
     Fk <- Fk/total
   }
 }
 return(cbind(k,Fk))
}
