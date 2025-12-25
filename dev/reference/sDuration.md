# Sample duration

Calculates the time represented by n samples in a Wave.

## Usage

``` r
sDuration(n = 1, wave = NULL, samp.rate = NULL)
```

## Arguments

- n:

  The number of the samples

- wave:

  A Wave object containing pulses

- samp.rate:

  Integer sampling rate

## Value

A numeric value in seconds

## Examples

``` r
sDuration(n=20, samp.rate=44100)
#> [1] 0.0004535147
if (FALSE) { # \dontrun{
sDuration(n=20, wave=sheep)#'
} # }

```
