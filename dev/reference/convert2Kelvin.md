# Convert temperature to Kelvin

Converts temperature measurements into Kelvin

## Usage

``` r
convert2Kelvin(temp, input = "C")
```

## Arguments

- temp:

  The value of the temperature to convert

- input:

  The unit of the temperature to convert, allowed values are "C", "F".

## Value

Numeric value in Kelvin

## Examples

``` r
convert2Kelvin(15, input="C")
#> [1] 288.15
convert2Kelvin(15, input="F")
#> [1] 263.7056
```
