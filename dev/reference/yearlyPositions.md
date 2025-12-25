# Generate positions of labels for a yearly plot

Generates positions for monthly labels of a dielPlot() in radians. The
positions can either be for the start of the month, or middle of the
month.

## Usage

``` r
yearlyPositions(year = 2022, format = "months")
```

## Arguments

- year:

  Year to calculate

- format:

  One of months, mid-months, days

## Details

The function allows for leap years if the year parameter is provided.
