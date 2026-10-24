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

  A Wave object

## Value

The shimmer in decibels

## Examples

``` r
if (FALSE) { # \dontrun{
shimmer(sheep)
} # }
```
