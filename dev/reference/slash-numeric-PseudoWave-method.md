# Numeric division by PseudoWave

A PseudoWave stands for (samples \* scale) + offset, and the reciprocal
of that is not of the same form, so dividing a number by a PseudoWave
cannot be held as a scale and an offset. The method exists to say so,
rather than leaving R to report that a PseudoWave is not a numeric
argument.

## Usage

``` r
# S4 method for class 'numeric,PseudoWave'
e1/e2
```

## Arguments

- e1:

  Input 1

- e2:

  Input 2

## Value

Nothing: this combination is always an error.
