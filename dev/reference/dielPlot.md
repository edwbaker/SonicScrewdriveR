# Create a diel plot

A diel plot shows the times of night, twilight and the maximum altitude
of the sun for a given date.

## Usage

``` r
dielPlot(
  date,
  lat,
  lon,
  limits = c(0, 2),
  plot = NULL,
  rot = tzRot(0),
  method = "plotrix",
  legend = F
)
```

## Arguments

- date:

  Date to plot.

- lat:

  Numeric latitude.

- lon:

  Numeric longitude.

- limits:

  Plotting limits of the daylight regions, default to c(1,2)

- plot:

  Character vector of components to plot

- rot:

  Either "Solar Noon" or an offset calculated by tz

- method:

  Plotting library to use

- legend:

  Whether to show a legend
