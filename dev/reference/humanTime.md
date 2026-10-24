# Converts time to human readable form

Given a time calculates the result in a sensible output unit (e.g.
minutes, hours).

## Usage

``` r
humanTime(S, unit = "seconds", digits = 3)
```

## Arguments

- S:

  Time to convert in unit. A vector may be given.

- unit:

  The unit of time to convert

- digits:

  Number of decimal places to round to, or NULL for no rounding.

## Value

String in human readable format, one for each value of S.

## Examples

``` r
humanTime(90)
#> [1] "1.5 minutes"
humanTime(c(1, 90, 7200))
#> [1] "1 second"    "1.5 minutes" "2 hours"    
humanTime(1, unit="hours")
#> [1] "1 hour"
```
