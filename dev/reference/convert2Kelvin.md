# Convert temperature to Kelvin

Converts temperature measurements into Kelvin

## Usage

``` r
convert2Kelvin(temp, input.unit = "C", input)
```

## Arguments

- temp:

  The value of the temperature to convert

- input.unit:

  The unit of the temperature to convert, one of "K", "C", "F".

- input:

  Deprecated, use input.unit instead.

## Value

Numeric value in Kelvin

## Examples

``` r
convert2Kelvin(15, input.unit="C")
#> [1] 288.15
convert2Kelvin(15, input.unit="F")
#> [1] 263.7056
```
