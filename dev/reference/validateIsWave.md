# Check an object is a Wave object

Helper function to test that the input is a Wave object. Will create an
error if not.

## Usage

``` r
validateIsWave(wave)
```

## Arguments

- wave:

  Object to test

## Value

The Wave object unchanged. An error is raised if it is not a Wave
object.

## Details

A warning of class `waveFormatMismatch` is raised where the wave's
samples do not fit the format it declares: a PCM wave holding samples
that are not whole numbers, or samples outside the range its bit depth
holds, or a floating point wave holding samples outside -1 to 1.
[`tuneR::Wave()`](https://rdrr.io/pkg/tuneR/man/Wave.html) declares
16-bit PCM unless told otherwise, so floating point samples given to it
without `bit=32, pcm=FALSE` make such a wave, and functions that cut,
filter or write it round each sample to a whole number. The check reads
1024 samples spread evenly over each channel, so that it takes the same
time however long the recording is, and can miss a fault confined to a
few samples.
