# Convert pressure to dyne per square centimetre

Converts pressure measurements into dyne per square centimetre

## Usage

``` r
convert2dyne_cm2(P, input = "kPa")
```

## Arguments

- P:

  The value of the pressure to convert

- input:

  The unit of the pressure to convert, allowed values are "kPa", "P".

## Examples

``` r
convert2dyne_cm2(1, input="Pa")
#> [1] 10
convert2dyne_cm2(1, input="kPa")
#> [1] 10000
```
