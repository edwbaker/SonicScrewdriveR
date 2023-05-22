#' Computes a beat spectrum
#'
#' Beat spectra represent the periodicity in signal amplitude.
#' It is computed by performing a continuous wavelet transform on
#' the envelope of a preprocessed signal, and processing
#' the average power per frequency band.
#'
#' @param wave an R object or path to a wave file
#' @param min_period the minimal rythmicity period expected, in seconds
#' @param max_period the maximal rythmicity period expected, in seconds
#' @param dj the frequency resolution of the cwt (in voices per octave)
#' @param ... extra arguments passed to \code{analyze.wavelet()}
#' @return a spectrum as a data frame.
#' It contains two columns: \code{power} and \code{period}.
#' The number of rows depend on the resolution and frequency range.
#' @importFrom stats approx runmed
#' @export
#' @author Quentin Geissmann
#' @examples
#' \dontrun{
#' beatSpectrum(sheep)
#' beatSpectrum(sheep, min_period=0.005, max_period=30, dj=1/32)
#' }
beatSpectrum <- function(wave,
                         min_period = 0.005,#s
                         max_period=30, #s,
                         dj=1/32, # 1/nvoices
                         ...
){

  #wave_std <- orthophonia::standardiseWave(wave)
  wave_std <- wave
  scaling_ratio <- wave_std@samp.rate / (1/min_period)
  runmed_k <- 2*(floor(scaling_ratio/2))+1
  signal <- runmed(abs(wave_std@left), runmed_k)
  n = length(signal)
  t0 <- 0:(n-1) / wave_std@samp.rate
  t1 <- seq(from=0, to=t0[length(t0)], by=min_period)
  signal_dsp <- approx(y=signal,t0, xout = t1)$y
  #dt_tmp <- data.table(x=signal_dsp)
  dt_tmp <- data.frame(x=signal_dsp)
  upper_period = ceiling(1 + log2(max_period/ min_period))
  wt <- WaveletComp::analyze.wavelet(dt_tmp,"x",
                        loess.span = 0, dj=dj,
                        lowerPeriod = 2 ^ 1,
                        upperPeriod = 2 ^ upper_period,
                        make.pval =F,
                        verbose = F,...)
  #data.table(power=wt$Power.avg, period = wt$Period * min_period)
  data.frame(power=wt$Power.avg, period = wt$Period * min_period)
}
