# Natural Time Domain

Runs a function on the wave and outputs values in the Natural Time
Domain (see Varotsos, Sarlis & Skordas(2011)
<doi:10.1007/978-3-642-16449-1>).

## Usage

``` r
ntd(wave, events, FUN, normalise = FALSE, argument = "wave", ...)
```

## Arguments

- wave:

  A Wave object containing pulses

- events:

  Onset of detected events, e.g. from pulseDetection()

- FUN:

  The function to run

- normalise:

  If TRUE the output is a probability density

- argument:

  If "wave" supplies a weave object to the function, if "vector"
  supplies the left channel as a numeric vector.

- ...:

  Additional arguments to FUN

## Value

A list of outputs form the applied function
