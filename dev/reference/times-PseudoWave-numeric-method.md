# PseudoWave scalar manipulation

A PseudoWave is applied as (samples \* scale) + offset, so multiplying
the whole of it by e2 multiplies the offset as well as the scale: ((g \*
a) + b) \* e2 is (g \* a\*e2) + b\*e2. Scaling only the scale left an
offset already set to be added after the multiplication rather than
taken into it, so the order the operators were written in was discarded.

## Usage

``` r
# S4 method for class 'PseudoWave,numeric'
e1 * e2
```

## Arguments

- e1:

  Input 1

- e2:

  Input 2

## Value

A PseudoWave object with its scale and its offset multiplied by e2.
