# Convert pressure to dyne per square centimetre

Converts pressure measurements into dyne per square centimetre

## Usage

``` r
convert2dyne_cm2(P, input.unit = "kPa", input)
```

## Arguments

- P:

  The value of the pressure to convert

- input.unit:

  The unit of the pressure to convert, one of "Pa", "kPa", "dyne_cm2".

- input:

  Deprecated, use input.unit instead.

## Value

The numeric value in dyne per square centimetre.

## Examples

``` r
convert2dyne_cm2(1, input.unit="Pa")
#> [1] 10
convert2dyne_cm2(1, input.unit="kPa")
#> [1] 10000
```
