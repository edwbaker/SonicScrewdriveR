# Various measurements of frequency values for a Wave object

Calculates the peak, centre, bandwidth and quality factor. The quality
factor (Q) is calculated at both -3dB and -10dB as discussed by
Bennett-Clark (1999) <doi:10.1080/09524622.1999.9753408>.

## Usage

``` r
entropyStats(wave)
```

## Arguments

- wave:

  A Wave object

## Value

A list of spectral entropy types.

## Examples

``` r
if (FALSE) { # \dontrun{
entropyStats(sheep)
} # }
```
