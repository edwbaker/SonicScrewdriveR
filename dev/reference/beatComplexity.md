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

  A Wave object

- plot:

  If TRUE a spectrogram overlaid with the peaks is plotted.

## Value

A list of the complexity, a vector of the peak periods, and the number
of peaks.

## Examples

``` r
if (FALSE) { # \dontrun{
  beatComplexity(sheep)
  beatComplexity(sheep, plot=TRUE)
} # }
```
