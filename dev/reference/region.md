# Specify a region with a file to analyse

Specifies a time-bounded region to analyse.

## Usage

``` r
region(time.unit, from = 0, to = Inf, unit)
```

## Arguments

- time.unit:

  Unit the from and to values are given in (one of samples, seconds,
  minutes, hours)

- from:

  Start time

- to:

  End time

- unit:

  Deprecated, use time.unit instead.

## Value

A TimeRegion object.
