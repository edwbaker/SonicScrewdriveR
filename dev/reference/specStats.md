# Calculate and plot statistics on a frequency spectrum

Given a list of outputs from meanspec generates a plot with the mean
shown by a line, and either the minimum/maximum values or one standard
deviation shown by a ribbon.

## Usage

``` r
specStats(spectra, stats = "minMax", line.col = "black", ribbon.col = "grey70")
```

## Arguments

- spectra:

  A list of spectra

- stats:

  Either minMax or sd

- line.col:

  Colour for the line

- ribbon.col:

  Colour for the ribbon

## Value

A ggplot2 object
