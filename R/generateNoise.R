generateNoise <- function(
  input, 
  noise = "white", 
  noiseAdd = FALSE, 
  noiseRatio=0.5, 
  output = "file",
  plot=FALSE) {
  
  if (typeof(input) == "S4") {
    input <- list(input)
  }
  
  noiseComponents <- noise
  if (noiseAdd) {
    noise <- paste(noise, collapse="+")
  }
  
  data <- list()
  
  for (i in 1:length(input)) {
    source <- mono(input[[i]])
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
        
        row <- list(noise=noise[[j]], noiseRatio=noiseRatio[[k]], wave=ratioNoise)
        
        data <- c(data, list(row))
      }
    }
  }
  return(data)
}