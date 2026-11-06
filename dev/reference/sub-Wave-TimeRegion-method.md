# Allow subsetting a Wave object with a TimeRegion

Applied to a `TaggedWave`, the cut is recorded in the `processing` slot
of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md),
with the `from`, `to` and `time.unit` of the region.

## Usage

``` r
# S4 method for class 'Wave,TimeRegion'
x[i]
```

## Arguments

- x:

  Wave Object

- i:

  TimeRegion object

## Value

A Wave object containing only the samples within the time region.
