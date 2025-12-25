# Upsample a wave

Used to upsample a Wave object. The upsampled sample rate must be an
natural multiple of the current sample rate.

## Usage

``` r
upsample(wave, upsample.rate, method = "basic")
```

## Arguments

- wave:

  Wave object to upsample.

- upsample.rate:

  The sample rate to upsample to.

- method:

  "basic" for linear, or a function to interpolate NAs in a vector

## Value

A resampled Wave object

## Examples

``` r
wave <- tuneR::sine(4000, samp.rate=44100)
wave2 <- upsample(wave, 88200)
```
