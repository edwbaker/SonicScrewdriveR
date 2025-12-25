# Apply a function to all channels of a Wave or WaveMC object

Some functions (e.g. ffilter from seewave) only operate on a single
channel at a time. This function applies the function to each channel
and returns a list of analyses.

## Usage

``` r
allChannels(
  w,
  FUN,
  cl = NULL,
  channel.param = "channel",
  output.FUN = NULL,
  ...
)
```

## Arguments

- w:

  A Wave or WaveMC object

- FUN:

  Function to apply to the wave.

- cl:

  Optionally a cluster for parallel calculation.

- channel.param:

  Name of the channel parameter to FUN. Can be NULL.

- output.FUN:

  Optional. Function that processes the output of FUN. The "channels_se"
  function provides standard functionality for the soundecology package.

- ...:

  Optional. Additional parameters to pass to FUN.

## Value

A list of outputs.
