# Cut wave by samples

Extract a section of a Wave object based on sample positions. This
function will automatically detect if a Wave object is stereo.

## Usage

``` r
cutws(wave, from = 1, to = Inf, plot = FALSE)
```

## Arguments

- wave:

  A Wave object

- from:

  First sample to return

- to:

  Last sample to return

- plot:

  If TRUE shows the cut region within the original waveform

## Value

A Wave object

## Examples

``` r
if (FALSE) { # \dontrun{
cutws(sheep, 1, 20)
cutws(sheep, 1, 20, plot=TRUE)
} # }
```
