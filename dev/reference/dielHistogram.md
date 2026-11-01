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
  limits = c(1, 2),
  rot = tzRot(0),
  timezone = NULL
)
```

## Arguments

- times:

  A vector of times that can be processed by dielFraction().

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

- rot:

  Rotation of the plot, which must match the dielPlot() being drawn on.

- timezone:

  The timezone to read the times in, which decides which bin each falls
  in. The default, NULL, uses the timezone the times themselves carry,
  and UTC for times that carry none.

## Value

A data frame of start and end points of bins.

## Details

Each bin is closed at its start and open at its end, as the data frame
returned labels it, so a time falling exactly on a boundary is counted
in the bin that begins there: a recording made at 01:00 is in the 01:00
bin and not in the midnight one.
