#' Computes a beat spectrum
#'
#' Beat spectra represent the periodicity in signal amplitude.
#' It is computed by performing a continuous wavelet transform on
#' the envelope of a preprocessed signal, and processing
#' the average power per frequency band.
#'
#' @param wave an R object or path to a wave file. Stereo Wave and WaveMC
#'   objects are passed to allChannels(), which returns the spectrum of each
#'   channel separately.
#' @param min_period the minimal rhythmicity period expected, in seconds
#' @param max_period the maximal rhythmicity period expected, in seconds, which
#'   is the longest period the spectrum runs to. It must be at least twice
#'   min_period.
#' @param dj the frequency resolution of the cwt (in voices per octave)
#' @param ... extra arguments passed to \code{analyze.wavelet()}
#' @return a spectrum as a data frame.
#' It contains two columns: \code{power} and \code{period}.
#' The number of rows depend on the resolution and frequency range.
#' For multi-channel input, one such data frame per channel.
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
  #Only the left slot of a Wave was read, so a stereo recording was analysed on
  #its left channel alone and a WaveMC, which has no such slot, failed outright.
  if (.useAllChannels(wave)) {
    #A closure keeps the arguments away from the formals of allChannels()
    return(allChannels(
      wave,
      function(w) beatSpectrum(w, min_period=min_period, max_period=max_period, dj=dj, ...),
      channel.param = NULL
    ))
  }

  #The envelope is resampled at min_period, and the shortest period the
  #transform can resolve in it is two of those steps.
  if (max_period < 2 * min_period) {
    stop(
      "max_period (", max_period, ") must be at least twice min_period (",
      min_period, "), the shortest period the spectrum can hold."
    )
  }

  #The envelope is smoothed over min_period and resampled at it, so a wave
  #shorter than the shortest period in the spectrum left an envelope of one or
  #two points. runmed() only warned that its window was longer than the wave,
  #and the transform failed with "missing value where TRUE/FALSE needed".
  #The tolerance keeps 441 samples at 44.1 kHz, which is twice the default
  #min_period, from being refused for the rounding of the product.
  if (length(wave@left) < 2 * min_period * wave@samp.rate - 1e-6) {
    stop(
      "The wave lasts ", signif(length(wave@left) / wave@samp.rate, 3), " s, ",
      "shorter than twice min_period (", 2 * min_period, " s), the shortest ",
      "period the spectrum can hold."
    )
  }

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
  #The longest period is max_period, in steps of the resampled envelope. It was
  #rounded up to a power of two, so the spectrum ran to as much as three times
  #the period asked for, and beatComplexity() could be set by a period the
  #caller had excluded. The power at each period is computed on its own, so the
  #periods that remain have the powers they had.
  wt <- WaveletComp::analyze.wavelet(dt_tmp,"x",
                        loess.span = 0, dj=dj,
                        lowerPeriod = 2 ^ 1,
                        upperPeriod = max_period / min_period,
                        make.pval =F,
                        verbose = F,...)
  #data.table(power=wt$Power.avg, period = wt$Period * min_period)
  data.frame(power=wt$Power.avg, period = wt$Period * min_period)
}
