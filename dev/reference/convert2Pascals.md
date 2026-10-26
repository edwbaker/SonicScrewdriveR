# Convert pressure to Pascals

Converts pressure measurements into Pascals

## Usage

``` r
convert2Pascals(P, input.unit = "kPa", input)
```

## Arguments

- P:

  The value of the pressure to convert

- input.unit:

  The unit of the pressure to convert, one of "Pa", "kPa", "dyne_cm2".

- input:

  Deprecated, use input.unit instead.

## Value

The numeric value in Pascals

## Examples

``` r
convert2Pascals(1000, input.unit="kPa")
#> [1] 1e+06
convert2Pascals(10, input.unit="dyne_cm2")
#> [1] 1
```
