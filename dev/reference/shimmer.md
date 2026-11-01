# Calculate the shimmer in a Wave object

Shimmer is a measure of the variability of amplitude between successive
periods in the waveform. It is returned in decibels, as the mean
absolute difference in level between the peak amplitudes of consecutive
periods. A waveform of constant amplitude has a shimmer of zero.

## Usage

``` r
shimmer(wave)
```

## Arguments

- wave:

  A Wave object. Stereo Wave and WaveMC objects are passed to
  allChannels(), which returns the shimmer of each channel separately.

## Value

The shimmer in decibels. For multi-channel input, one such value per
channel.

## Examples

``` r
if (FALSE) { # \dontrun{
shimmer(sheep)
} # }
```
