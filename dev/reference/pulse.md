# Generate a single pulse

Generate a single pulse, either a Dirac pulse (Dirac delta) or a square
pulse.

## Usage

``` r
pulse(
  type = "dirac",
  leading = 22050,
  pulse.length = 1,
  duration = samp.rate,
  samp.rate = 44100,
  bit = 1,
  pcm = FALSE,
  stereo = FALSE,
  output = "Wave",
  invert = FALSE
)
```

## Arguments

- type:

  Either "dirac" or "square".

- leading:

  The number of samples before the pulse.

- pulse.length:

  The number of samples in the pulse (for "square").

- duration:

  The total number of samples generated.

- samp.rate:

  The sample rate.

- bit:

  The bit depth.

- pcm:

  Whether Wave generated is PCM (see tuneR).

- stereo:

  Whether Wave generated is stereo.

- output:

  The output format ("Wave").

- invert:

  Whether to invert the pulse.

## Value

Specified by output.
