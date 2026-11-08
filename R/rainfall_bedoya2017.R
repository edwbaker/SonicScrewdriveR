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

#' Refuse a wave whose 600-1200 Hz band has too few bins to measure
#'
#' The ratio Bedoya et al. (2017) threshold is taken across the bins of the
#' band, and a standard deviation needs two of them. A bin of a 512-sample
#' window is the sample rate over 512 wide, so from 204.8 kHz up the band
#' holds one at most: bat detectors record at 250 and 384 kHz. A wave sampled
#' too slowly to reach the band has too few bins in it for the opposite reason,
#' and is told so.
#' @param rate The sample rate of the wave, in Hz.
#' @param bins How many bins the band holds.
#' @noRd
.bandTooNarrow <- function(rate, bins) {
  hz <- function(x) format(x, scientific=FALSE, trim=TRUE)
  if (rate / 2 < 1200) {
    stop(
      "The method of Bedoya et al. (2017) measures the 600-1200 Hz band, and a wave ",
      "sampled at ", hz(rate), " Hz reaches only ", hz(rate / 2), " Hz.",
      call.=FALSE
    )
  }
  stop(
    "The method of Bedoya et al. (2017) cannot be applied at a sample rate of ",
    hz(rate), " Hz. Its 600-1200 Hz band holds ", bins,
    if (bins == 1) " frequency bin" else " frequency bins",
    " of the 512-sample windows it uses, which are ", hz(signif(rate / 512, 4)),
    " Hz wide at that rate, and the ratio of the band's mean to its standard ",
    "deviation needs at least two. Downsampled to 48 kHz, for example, the band ",
    "holds six.",
    call.=FALSE
  )
}

rainfall_bedoya2017 <- function(wave, Tmean=0.9e-6, Tsnr=3.5) {
  a <- .bandPSD(wave, 600, 1200)
  #Checked before the statistics rather than after, and for any band too small to
  #have a standard deviation rather than only for one of exactly one row. An empty
  #band gave a mean of NaN, which was then compared against the thresholds.
  #It is an error, rather than a message and NULL as it was. A message is lost
  #on the workers of a cluster, and a pipeline takes NULL for a step with
  #nothing to report, so a recording that could not be measured was stored as
  #one that had been, with no row to say otherwise.
  if (length(a) < 2) {
    .bandTooNarrow(wave@samp.rate, length(a))
  }
  mean_a <- mean(a)
  #Across the band's bins of a spectrum already averaged over the recording,
  #which makes this rise with the recording's length as the averaging shrinks the
  #standard deviation: the same noise gives 4.0 over 0.05 s and 49 over 60 s, so
  #Tsnr holds for a clip length rather than absolutely. Taking the ratio within
  #each window instead and pooling, which is length-independent and is what the
  #hardRain package does, was measured on labelled recordings and discriminates
  #nothing: rain 1.12 to 1.17, quiet 1.01 to 1.06, noise 1.21, all on top of each
  #other, where this measure gives rain 11 to 27 against 1.4 to 5.8. It is kept
  #for that reason and not by oversight.
  std_a <- stats::sd(a)
  c <- mean_a/std_a
  if (mean_a > Tmean && c >Tsnr) {
    r <- mean_a
  } else {
    r <- 0
  }
  return(c(r))
}
