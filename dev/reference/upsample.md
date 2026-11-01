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

A resampled Wave object of the same format as the input

## Details

Applied to a `TaggedWave` the call is recorded in the `processing` slot
of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).

Interpolation between two samples lands between them, so where the wave
declares integer PCM the interpolated samples are rounded to whole
numbers. A floating point wave is interpolated without rounding.

## Examples

``` r
wave <- tuneR::sine(4000, samp.rate=44100)
wave2 <- upsample(wave, 88200)
```
