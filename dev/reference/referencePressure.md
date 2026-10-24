# Reference pressure

Provides the standard reference pressure level.

## Usage

``` r
referencePressure(unit = "Pa")
```

## Arguments

- unit:

  Unit to return, "Pa" or "dyne_cm2"

## Value

The reference sound pressure, in the unit specified.

## Examples

``` r
rp <- referencePressure()
rp <- referencePressure(unit="dyne_cm2")
```
