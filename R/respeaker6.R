#' Read a file from Seeed Studio Respeaker 6 mic array
#'
#' The Seeed Studio Respeaker-6 when used as described in the documentation saves an eight
#' channel audio file with channels 3 and 4 not containing input audio. This function reads
#' such a file and saves it as a six channel file.
#'
#' @param filename file to read
#' @param from See readWave
#' @param to See readWave
#' @param units See readWave
#' @param header See readWave
#' @return Missing value of n or t
#' @export
#' @importFrom tuneR readWave
readRespeaker6 <- function(filename, from=1, to=Inf, units="samples", header=FALSE) {
  w <- readWave(filename, from, to, units, header, toWaveMC=TRUE)
  w@.Data[which(is.na(w@.Data))] <- 0
  w@.Data <- w@.Data[,1:6]
  colnames(w@.Data) <- c("FL", "FR", "FC", "BL", "BR", "BC")
  return(w)
}
