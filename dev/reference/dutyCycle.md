# Calculate the duty cycle of a wave

Proportion of a wave with signal above the limit

## Usage

``` r
dutyCycle(wave, limit = 0.1, output = "unit", normalise = TRUE)
```

## Arguments

- wave:

  A Wave object

- limit:

  Threshold above which to consider the signal

- output:

  If "unit" the duty cycle will be in the range 0-1. For a percentage
  use "percent".

- normalise:

  If TRUE the Wave is normalised using tuneR

## Value

A numerical value for the duty cycle between 0 and 1 (or 0 and 100% if
percentage output).

## Examples

``` r
wave <- tuneR::sine(2000)
dc <- dutyCycle(wave)
pc <- dutyCycle(wave, output="percent")
```
