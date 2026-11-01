# Beat spectrum complexity

This function computes a `beatSpectrum` and calculates some basic
measurements of its complexity. The complexity value is calculated as
the maximum identified repeating period (in seconds) divided by the
number of peaks.

## Usage

``` r
beatComplexity(wave, plot = FALSE)
```

## Arguments

- wave:

  A Wave object. Stereo Wave and WaveMC objects are passed to
  allChannels(), which measures each channel separately.

- plot:

  If TRUE a spectrogram overlaid with the peaks is plotted. One plot is
  drawn per channel, so a multi-channel wave needs a layout of its own
  for all of them to be seen.

## Value

A list of the complexity, a vector of the peak periods, and the number
of peaks. The complexity is NA if the beat spectrum has no peak. For
multi-channel input, one such list per channel.

## Examples

``` r
if (FALSE) { # \dontrun{
  beatComplexity(sheep)
  beatComplexity(sheep, plot=TRUE)
} # }
```
