# Convert file sizes to bytes

Converts file size measurements into bytes. Both decimal units, which
are powers of 1000, and binary units, which are powers of 1024, are
accepted.

## Usage

``` r
convert2bytes(S, input = "bits")
```

## Arguments

- S:

  The value to convert

- input:

  The unit to convert, one of those given by fileSizeUnits(), or "bits"
  or "bytes".

## Value

The numeric value in bytes

## Examples

``` r
convert2bytes(8, input="bits")
#> [1] 1
convert2bytes(1, input="kB")
#> [1] 1000
convert2bytes(1, input="KiB")
#> [1] 1024
```
