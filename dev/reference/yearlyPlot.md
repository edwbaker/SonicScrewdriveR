# Create a yearly plot

ToDO......

## Usage

``` r
yearlyPlot(
  year = 2022,
  lat,
  lon,
  limits = c(0, 2),
  plot = NULL,
  method = "plotrix",
  legend = FALSE
)
```

## Arguments

- year:

  Year to plot (allows for leap years).

- lat:

  Numeric latitude.

- lon:

  Numeric longitude.

- limits:

  Plotting limits of the daylight regions, default to c(1,2)

- plot:

  Character vector of components to plot

- method:

  Plotting library to use

- legend:

  Whether to show a legend

## Value

Called for its side effect of drawing a plot. The return value is that
of the underlying plotting function and should not be relied on.
