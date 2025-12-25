# Samples per time period

Calculates the number of samples for a given duration of a wave

## Usage

``` r
tSamples(time = 1, wave = NULL, samp.rate = NULL)
```

## Arguments

- time:

  The duration in seconds

- wave:

  A Wave object containing pulses

- samp.rate:

  Integer sampling rate

## Value

Number of samples

## Examples

``` r
tSamples(10, samp.rate=44100)
#> [1] 441000
if (FALSE) { # \dontrun{
tSamples(10, wave=sheep)
} # }

```
