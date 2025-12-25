# Convert pressure to Pascals

Converts pressure measurements into Pascals

## Usage

``` r
convert2Pascals(P, input = "kPa")
```

## Arguments

- P:

  The value of the pressure to convert

- input:

  The unit of the pressure to convert, allowed values are "kPa",
  "dyne_cm2".

## Value

The numeric value in Pascals

## Examples

``` r
convert2Pascals(1000, input="kPa")
#> [1] 1e+06
convert2Pascals(10, input="dyne_cm2")
#> [1] 1
```
