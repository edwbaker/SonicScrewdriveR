# Reference pressure

Provides the standard reference pressure level.

## Usage

``` r
referencePressure(output.unit = "Pa", unit)
```

## Arguments

- output.unit:

  Unit to return, "Pa" or "dyne_cm2"

- unit:

  Deprecated, use output.unit instead.

## Value

The reference sound pressure, in the unit specified.

## Examples

``` r
rp <- referencePressure()
rp <- referencePressure(output.unit="dyne_cm2")
```
