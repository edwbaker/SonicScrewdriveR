# Converts bytes in human readable form

Given an input of bytes calculates the result in a sensible output unit
(e.g. MB, GB, PB). Decimal units are powers of 1000, and binary units
powers of 1024.

## Usage

``` r
humanBytes(S, units = "decimal", digits = 3)
```

## Arguments

- S:

  Number of bytes. A vector may be given.

- units:

  One of "decimal", giving units such as kB, or "binary", giving units
  such as KiB.

- digits:

  Number of decimal places to round to, or NULL for no rounding.

## Value

String in human readable format, one for each value of S.

## Examples

``` r
humanBytes(1500)
#> [1] "1.5 kB"
humanBytes(1500, units="binary")
#> [1] "1.465 KiB"
humanBytes(c(1, 1024, 1e6))
#> [1] "1 byte"   "1.024 kB" "1 MB"    
```
