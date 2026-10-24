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
  legend = TRUE,
  rot = tzRot(0)
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

- rot:

  Rotation of the plot, which must match the dielPlot() being drawn on.

## Value

Called for its side effect of drawing on the current plot. The return
value is that of the underlying plotting function and should not be
relied on.
