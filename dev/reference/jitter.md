# Calculate the jitter in a Wave object

Jitter is a measure of the variability of the length of successive
periods in the waveform. Absolute jitter is the mean absolute difference
between the lengths of consecutive periods, in samples; relative jitter
divides this by the mean period length. A waveform of constant period
has a jitter of zero.

## Usage

``` r
jitter(wave, method = "absolute")
```

## Arguments

- wave:

  A Wave object

- method:

  One of "absolute" or "relative"

## Value

The jitter, in samples for method "absolute" and as a proportion of the
mean period for method "relative"

## Details

Periods are located from zero crossings, so they are measured to the
nearest sample. A tone whose period is not a whole number of samples
will report a small amount of jitter for this reason.

## Examples

``` r
if (FALSE) { # \dontrun{
jitter(sheep, method="absolute")
jitter(sheep, method="relative")
} # }
```
