# Diel Histogram

Draws a histogram on a dielPlot() using pre-defined bins related to time
of day.

## Usage

``` r
dielHistogram(
  times,
  by = "hour",
  col = "grey",
  maxval = NA,
  presence.only = FALSE,
  limits = c(1, 2)
)
```

## Arguments

- times:

  A vector of times that can be pocessed by dielFraction().

- by:

  Controls the size of histogram bins, one of "hour", "15minute",
  "30minute".

- col:

  Colour of the plot.

- maxval:

  By default scales histogram within limits, specifying a maximum value
  here allows comparison between plots.

- presence.only:

  Only show presence/absence not values.

- limits:

  Limits of the plotting (see dielPlot()).

## Value

A data frame of start and end points of bins.
