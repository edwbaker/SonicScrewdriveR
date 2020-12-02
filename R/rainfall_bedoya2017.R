rainfall_bedoya2017 <- function(wave, Tmean=1e-6, Tsnr=3.5) {
  p <- seewave::meanspec(wave, f=wave@samp.rate, wl=512, PSD=TRUE, plot=FALSE, norm=FALSE, ovlp=0)
  a <- subset(p, p[,1] > 0.6 & p[,1] < 1.2)
  mean_a <- mean(a[,2])
  std_a <- stats::sd(a[,2])
  if (nrow(a) == 1) {
    message("Not enough data to create standard deviation")
    return(NULL)
  }
  c <- mean_a/std_a
  if (mean_a > Tmean && c >Tsnr) {
    r <- mean_a
  } else {
    r <- 0
  }
  return(c(r))
}
