#' Read a file from Seeed Studio Respeaker 6 mic array
#'
#' The Seeed Studio Respeaker-6 when used as described in the documentation saves an eight
#' channel audio file with channels 7 and 8 not containing input audio. This function reads
#' such a file and saves it as a six channel file.
#'
#' @param filename file to read.
#' @param from Where to start reading the wave in input.unit.
#' @param to Where to stop reading the wave in input.unit.
#' @param input.unit Unit in which from and to is given, the default is "samples", but can be set to time intervals such as "seconds".
#' @param units Deprecated, use input.unit instead.
#' @param header If TRUE, just header information of the Wave file are returned, otherwise (the default) the whole Wave object.
#' @return A WaveMC object.
#' @export
#' @importFrom tuneR readWave
readRespeaker6 <- function(filename, from=1, to=Inf, input.unit="samples", header=FALSE, units) {
  if (!missing(units)) {
    .deprecatedArg("units", "input.unit", "readRespeaker6")
    input.unit <- units
  }
  input.unit <- .validateUnit(input.unit, .wavePositionUnits(), "readRespeaker6")
  w <- readWave(filename, from=from, to=to, units=input.unit, header=header, toWaveMC=TRUE)
  w@.Data[which(is.na(w@.Data))] <- 0
  w@.Data <- w@.Data[,1:6]
  colnames(w@.Data) <- c("FL", "FR", "FC", "BL", "BR", "BC")
  return(w)
}
