# Units used for file sizes

The units understood by convert2bytes() and produced by humanBytes().
Decimal units are powers of 1000 and binary units are powers of 1024.

## Usage

``` r
fileSizeUnits(units = "decimal")
```

## Arguments

- units:

  One of "decimal" or "binary"

## Value

A character vector of unit names, from smallest to largest.

## Examples

``` r
fileSizeUnits()
#> [1] "kB" "MB" "GB" "TB" "PB" "EB"
fileSizeUnits("binary")
#> [1] "KiB" "MiB" "GiB" "TiB" "PiB" "EiB"
```
