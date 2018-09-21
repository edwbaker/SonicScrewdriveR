#' Windowing Function for Wave Objects
#'
#' Seperates a Wave object into windows of a defined length and runs a function on the window section. Windows may overlap, and the function can make use of 'parallel' package for multicore processing.
#' 
#' @param wave A Wave object
#' @param window.length The lag used to create the A-matrix
#' @param window.overlap A matrix used to code the Duration-Shape pairs
#' @param max_D The maximum Duration to code
#' @param FUN If TRUE plots the workings of the coding algorithm
#' @param ... Additional parameters to FUN
#' @param cluster A cluser form the 'parallel' package for multicore computation
#' @import seewave
#' @import tuneR
#' @keywords wave
#' @export
#' @examples
#' library(tuneR)
#' wave <- readWave(system.file("extdata", "1.wav", package="tdsc"))
#' t <- tdsc(wave)

generateNoise <- function(
  wave, 
  noise = "white", 
  noiseAdd = FALSE, 
  noiseRatio=0.5, 
  output = "file",
  plot=FALSE) {
  
  seewave_avail <- requireNamespace("seewave", quietly=TRUE)

  input <- c()
  for (i in 1:length(files)) {
    if (file.exists(files[[i]])) {
      input <- c(input, files[[i]])
    } else {
      warning(paste("File:", files[[i]], "could not be found."))
    }
  }
  if (length(input) == 0) {
    stop("No source files specified.")
  }
  
  noiseComponents <- noise
  if (noiseAdd) {
    noise <- paste(noise, collapse="+")
  }
  
  data <- c()
  
  for (i in 1:length(input)) {
    source <- readWave(input[[i]])
    source <- normalize(source)
    source_d <- duration(source)
    if (plot==TRUE) {
      oscillo(source)
    }
    for (j in 1:length(noise)) {
      noises <- silence(duration=source_d, samp.rate = source@samp.rate, bit = source@bit, pcm=source@pcm, xunit="time")
      for (m in 1:length(noiseComponents)) {
        if (noiseComponents[[m]] %in% c("unif", "gaussian")) {
          n <- noisew(source@samp.rate, source_d, type=noiseComponents[[m]], output="Wave")
          n <- normalize(n)
          noises <- noises + n
          next()
        }
        if (noiseComponents[[m]] %in% c("white", "pink", "power", "red")) {
          n <- noise(kind=noiseComponents[[m]], duration=source_d, samp.rate=source@samp.rate, bit=source@bit, pcm=source@pcm, xunit="time")
          n <- normalize(n)
          noises <- noises + n
          next()
        }
        if (noiseComponents[[m]] %in% c("rain", "thunder", "wind")) {
          
          next()
        }
        if (is.numeric(noiseComponents[[m]])) {
          n <- sine(noiseComponents[[m]], duration=source_d, samp.rate=source@samp.rate, bit=source@bit, pcm=source@pcm, xunit="time")
          n <- normalize(n)
          noises <- noises + n
          next()
        }
        if (file.exists(noiseComponents[[m]])) {
          nf <- readWave(noiseComponents[[m]], from=0, to=source_d, units="seconds")
          nf <- normalize(nf)
          nf_d <- duration(nf)
          
          n <- nf
          while (duration(n) < source_d) {
            n <- bind(n, nf)
          }
          n <- cutw(n, nf@samp.rate, from=0, to=source_d, output="Wave")
          
          noises <- noises + n
          next()
          
        }
        
      }
      noises <-normalize(noises)
      if (plot==TRUE) {
        oscillo(noises)
      }
      for (k in 1:length(noiseRatio)) {
        ratioNoise <- source + noises * noiseRatio[[k]]
        if (plot==TRUE) {
          oscillo(ratioNoise)
        }
        
        row <- c(input[[i]], noise[[j]], noiseRatio[[k]])
        

        if (output == "file") {
          filename <- paste0(c(row, ".wav"), collapse="_")
          savewav(ratioNoise, source@samp.rate, filename=filename)
        }
        if (output == "Wave") {
          row <- c(row, ratioNoise)
        }
        
        data <- rbind(data, row)
      }
    }
  }
  return(data)
}