# Rainfall detection

Detects rainfall in a Wave. An uncalibrated version of Bedoya et al
(2017) <doi:10.1016/j.ecolind.2016.12.018> is available in this package.
The hardRain package can also be accessed via this wrapper.

## Usage

``` r
rainfallDetection(wave, method = "bedoya2017", ...)
```

## Arguments

- wave:

  A Wave object to detect rainfall in

- method:

  Which rainfall detection method to use ("bedoya2017")

- ...:

  Other arguments to pass to rain detection function

## Value

Numeric value from the rainfall detection algorithm chosen.

## Examples

``` r
if (FALSE) { # \dontrun{
rainfallDetection(sheep, method="bedoya2017")
} # }
```
