beatComplexity <-function(
  wave,
  plot=FALSE
) {
  bs <- beatSpectrum(wave) 
  #bs$power <- bs$power / max(bs$power)
  
  e <- c(0,diff(sign(diff(bs$power))),0)
  

  
  peaks <- e==-2
  
  if (plot) {
    plot(bs$period, bs$power, type="l")  
    abline(v=bs$period[peaks], col="green")
  }
  
  c <- max(bs$period[peaks]) / length(peaks)
  
  return(c)
  
}