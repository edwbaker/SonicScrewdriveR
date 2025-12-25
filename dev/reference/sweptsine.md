# Generate a sine sweep

Generates a frequency swept sine wave (either linear or logarithmic) and
returns it as a Wave object or vector.

## Usage

``` r
sweptsine(
  f0 = 100,
  f1 = 2500,
  mode = "linear",
  sweep.time = 1,
  time.unit = "seconds",
  samp.rate = 44100,
  output = "wave",
  ...
)
```

## Arguments

- f0:

  Start frequency

- f1:

  End frequency

- mode:

  One of "linear", "log"

- sweep.time:

  Duration of swept wave

- time.unit:

  One of "seconds", "samples"

- samp.rate:

  Sample rate of swept wave

- output:

  "wave" for a Wave object, or "vector"

- ...:

  Additional arguments to pass to data2Wave

## Value

A swept wave object of the type specified in output.

## Examples

``` r
#Generate a swept sine wave between 0Hz and 10kHz.
w <- sweptsine(0, 10e3)

#Generate a swept sine wave between 0Hz and 10kHz and normalise it.
w <- normalise(sweptsine(0, 10e3))

#Generate a stereo swept sine wave between 100Hz and 1KHz.
w <- tuneR::stereo(sweptsine(100, 1e3))

#Generate an exponentially swept sine wave between 100Hz and 1KHz.
w <- sweptsine(100, 1e3, mode="log")
```
