# Convert temperature to Celsius

Converts temperature measurements into Celsius

## Usage

``` r
convert2Celsius(temp, input = "K")
```

## Arguments

- temp:

  The value of the temperature to convert

- input:

  The unit of the temperature to convert, allowed values are "K", "F".

## Value

Numeric value in degrees Celsius

## Examples

``` r
convert2Celsius(15, input="K")
#> [1] -258.15
convert2Celsius(15, input="F")
#> [1] -9.444444
```
