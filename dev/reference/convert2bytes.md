# Convert file sizes to bytes

Converts file size measurements into bytes. Both decimal units, which
are powers of 1000, and binary units, which are powers of 1024, are
accepted.

## Usage

``` r
convert2bytes(S, input.unit = "bits", input)
```

## Arguments

- S:

  The value to convert

- input.unit:

  The unit to convert, one of those given by fileSizeUnits(), or "bits"
  or "bytes".

- input:

  Deprecated, use input.unit instead.

## Value

The numeric value in bytes

## Examples

``` r
convert2bytes(8, input.unit="bits")
#> [1] 1
convert2bytes(1, input.unit="kB")
#> [1] 1000
convert2bytes(1, input.unit="KiB")
#> [1] 1024
```
