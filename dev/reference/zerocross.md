# Identify zero crossings in a Wave object

Returns a vector of the position (in samples) of zero crossings in a
Wave object

## Usage

``` r
zerocross(wave)
```

## Arguments

- wave:

  A Wave object. Stereo Wave and WaveMC objects are passed to
  allChannels(), which returns the crossings of each channel separately.

## Value

A vector of zero crossing locations. For multi-channel input, one such
vector per channel.

## Examples

``` r
if (FALSE) { # \dontrun{
zerocross(sheep)
} # }
```
