# Calculate the jitter in a Wave object

Jitter is a measure of the variability of periods in the waveform.
Relative jitter is scaled by the jitter in the analysed waveform.

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

A vector of zero crossing locations

## Examples

``` r
if (FALSE) { # \dontrun{
jitter(sheep, method="absolute")
jitter(sheep, method="relative")
} # }
```
