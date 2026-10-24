# Plot a false-colour index spectrogram

Plot a false-colour index spectrogram

## Usage

``` r
# S3 method for class 'fcis'
plot(
  x,
  xlab = NULL,
  ylab = "Frequency (kHz)",
  main = "False-colour index spectrogram",
  ...
)
```

## Arguments

- x:

  An object of class "fcis"

- xlab:

  Label for the x axis

- ylab:

  Label for the y axis

- main:

  Title for the plot

- ...:

  Additional parameters passed to plot()

## Value

No return value, called for its side effect of drawing a plot.

## Examples

``` r
if (FALSE) { # \dontrun{
plot(fcis(wave, window.length=44100))
} # }
```
