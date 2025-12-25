# Add two spectra from seewave

This function takes two spectra from seewave (or equivalent) and adds
their values. The spectra must have the same bins.

## Usage

``` r
addSpectra(s1, s2, coerceNegative = TRUE)
```

## Arguments

- s1:

  First spectrum

- s2:

  Second spectrum

- coerceNegative:

  Sets any values below zero to zero in output.

## Value

A spectrum of s1+s2

## Examples

``` r
if (FALSE) { # \dontrun{
subtractSpectra(spec1, spec2)
} # }
```
