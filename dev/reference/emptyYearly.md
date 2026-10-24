# Create an empty yearly plot

Create a yearly plot with labels but without sun or night duration
plotted.

## Usage

``` r
emptyYearly(year = 2022, method = "plotrix", rot = pi)
```

## Arguments

- year:

  Year to plot (allows for leap years)

- method:

  Plotting package to use

- rot:

  Rotation of the origin (defaults to pi)

## Value

Called for its side effect of drawing a plot. The return value is that
of the underlying plotting function and should not be relied on.
