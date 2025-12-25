# Plot rings on a diel plot

Plot rings on a diel plot.

## Usage

``` r
dielRings(
  names,
  starts,
  ends,
  cols = "grey",
  format = "HHMM",
  limits = c(1, 2),
  legend = T
)
```

## Arguments

- names:

  Labels for the rings

- starts:

  Start times for rings in HHMM string format

- ends:

  End times for rings in HHMM string format

- cols:

  Colours of the rings

- format:

  Defaults to HHMM

- limits:

  Region of a dielPlot() to plot rings. Defaults to c(1,2)

- legend:

  Boolean. Whether to plot a legend.
