# Convert temperature to Celsius

Converts temperature measurements into Celsius

## Usage

``` r
convert2Celsius(temp, input.unit = "K", input)
```

## Arguments

- temp:

  The value of the temperature to convert

- input.unit:

  The unit of the temperature to convert, one of "K", "C", "F".

- input:

  Deprecated, use input.unit instead.

## Value

Numeric value in degrees Celsius

## Examples

``` r
convert2Celsius(15, input.unit="K")
#> [1] -258.15
convert2Celsius(15, input.unit="F")
#> [1] -9.444444
```
