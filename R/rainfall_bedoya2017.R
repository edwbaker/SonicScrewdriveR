#' Power spectral density of a frequency band, relative to full scale
#'
#' Bedoya et al. (2017) threshold the mean power spectral density of the
#' 600-1200 Hz band at 0.9e-6, which presumes a quantity whose scale is fixed.
#' `meanspec(PSD=TRUE, norm=FALSE)` gives neither: it leaves the result in the
#' sample units of the wave, so the same recording read at 8 bit, 16 bit and
#' 32 bit gave values fourteen orders of magnitude apart and no threshold could
#' mean anything, and it averages the amplitude over the window before squaring
#' it, so the result is a squared mean amplitude rather than a mean power.
#'
#' This divides the samples by the largest value the format holds, averages the
#' power of each bin over the windows, and divides by the bin width. The result
#' is power per Hz relative to full scale: the same number whatever the bit
#' depth, and on the scale the published threshold is in.
#'
#' @param wave A mono Wave object.
#' @param lo,hi The band to return, in Hz.
#' @param wl Window length in samples.
#' @param ovlp Percentage overlap between windows.
#' @param wn Window name, as seewave::ftwindow() names them.
#' @return The power spectral density of the band's bins, in power per Hz.
#' @importFrom seewave ftwindow spectro
#' @noRd
.bandPSD <- function(wave, lo, hi, wl=512, ovlp=0, wn="hanning") {
  x <- as.numeric(wave@left)
  if (wave@pcm && wave@bit == 8) {
    #Eight bit PCM is unsigned about a midpoint of 128, so the 255 that
    #.waveFullScale() gives is the largest sample rather than the largest
    #excursion from silence. Dividing by it without centring first halves the
    #amplitude and so quarters the power, which put an eight bit wave four times
    #below the same audio held at any other depth.
    x <- (x - 128) / (255 - 128)
  } else {
    x <- x / .waveFullScale(wave)
  }
  win <- seewave::ftwindow(wl, wn=wn)
  s <- seewave::spectro(
    x,
    f=wave@samp.rate,
    wl=wl,
    wn=wn,
    ovlp=ovlp,
    norm=FALSE,
    dB=NULL,
    plot=FALSE,
    correction="none"
  )
  #spectro() returns 2*Mod(fft)/wl for each bin, so a bin holds (amp*wl/2)^2 of
  #squared magnitude, and the one-sided density is twice that over the sample
  #rate and the energy of the window. Summing the whole spectrum and multiplying
  #by the bin width recovers the variance of x, which is the check that this
  #scaling is right rather than merely consistent.
  psd <- rowMeans(s$amp^2) * wl^2 / (2 * wave@samp.rate * sum(win^2))
  #spectro() gives the frequencies of its bins in kHz.
  freq <- s$freq * 1000
  return(psd[freq > lo & freq < hi])
}

rainfall_bedoya2017 <- function(wave, Tmean=0.9e-6, Tsnr=3.5) {
  a <- .bandPSD(wave, 600, 1200)
  #Checked before the statistics rather than after, and for any band too small to
  #have a standard deviation rather than only for one of exactly one row. An empty
  #band gave a mean of NaN, which was then compared against the thresholds.
  if (length(a) < 2) {
    message("Not enough data to create standard deviation")
    return(NULL)
  }
  mean_a <- mean(a)
  std_a <- stats::sd(a)
  c <- mean_a/std_a
  if (mean_a > Tmean && c >Tsnr) {
    r <- mean_a
  } else {
    r <- 0
  }
  return(c(r))
}
