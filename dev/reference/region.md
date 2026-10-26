# Specify a region with a file to analyse

Specifies a time-bounded region to analyse.

## Usage

``` r
region(input.unit, from = 0, to = Inf, unit)
```

## Arguments

- input.unit:

  Unit the from and to values are given in (one of samples, seconds,
  minutes, hours)

- from:

  Start time

- to:

  End time

- unit:

  Deprecated, use input.unit instead.

## Value

A TimeRegion object.
