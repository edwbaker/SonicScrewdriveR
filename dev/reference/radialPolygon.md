# Plot a radial polygon

Used to plot sectors, annuli and horizons on a dielPlot() or
yearlyPlot(). The polygon has an inner and outer horizon - which can be
set to a fixed radius or a vector.

## Usage

``` r
radialPolygon(
  angle1,
  angle2,
  radius1,
  radius2,
  col = "grey",
  border = NA,
  rot = -pi,
  angleinc = 0.01,
  reverse = TRUE,
  ...
)
```

## Arguments

- angle1:

  Angles for the inner line

- angle2:

  Angles for the outer line

- radius1:

  Radii for the inner line

- radius2:

  Radii for the outer line

- col:

  Colour of the polygon

- border:

  Border colour (see polygon() for details)

- rot:

  Rotation of the plot, defaults to pi to match dielPlot() and
  yearlyPlot()

- angleinc:

  The angular increment in radians for calculating circular lines

- reverse:

  If FALSE plots in an anti-clockwise direction

- ...:

  Other parameters passed to polygon()
