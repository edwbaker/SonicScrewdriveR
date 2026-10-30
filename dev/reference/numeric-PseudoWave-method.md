# Numeric subtraction by PseudoWave

A PseudoWave is applied as (samples \* scale) + offset, so subtracting
one from a number negates the scale and takes the offset from the
number: e1 - ((g \* scale) + offset) is (g \* -scale) + (e1 - offset).

## Usage

``` r
# S4 method for class 'numeric,PseudoWave'
e1 - e2
```

## Arguments

- e1:

  Input 1

- e2:

  Input 2

## Value

A PseudoWave object giving e1 minus the wave e2 stands for.
