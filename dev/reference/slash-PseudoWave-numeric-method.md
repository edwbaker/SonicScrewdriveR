# PseudoWave scalar division

Dividing the whole of a PseudoWave by e2 divides the offset as well as
the scale, for the reason given for multiplication above.

## Usage

``` r
# S4 method for class 'PseudoWave,numeric'
e1/e2
```

## Arguments

- e1:

  Input 1

- e2:

  Input 2

## Value

A PseudoWave object with its scale and its offset divided by e2.
