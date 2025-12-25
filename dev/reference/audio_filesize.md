# Calculated size of raw audio files

Calculates the raw size of audio date at set sample rate, bit depth and
duration.

## Usage

``` r
audio_filesize(
  samp.rate = 44100,
  bit.depth = 16,
  channels = 1,
  duration = 1,
  duration.unit = "seconds",
  output.unit = "bits"
)
```

## Arguments

- samp.rate:

  Sample rate

- bit.depth:

  Bit depth

- channels:

  The number of audio channels

- duration:

  Duration of recording

- duration.unit:

  One of seconds, minutes, hours, days

- output.unit:

  "human", "bits" or "bytes"

## Value

The size of the audio file in the specified unit

## Details

By default
[`humanBytes()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/humanBytes.md)
is used to convert the output to human readable format, however this can
be changed by setting `output.unit` to "bits" or "bytes".

## Examples

``` r
# One minute of mono 16-bit audio sampled at 44.1kHz
audio_filesize(samp.rate=44100, bit.depth=16, channels=1, duration=1, duration.unit="minutes")
#> [1] 42336000

# One year of stereo 24-bit audio sampled at 96kHz
audio_filesize(samp.rate=96000, bit.depth=24, channels=2, duration=1, duration.unit="years")
#> [1] 1.453179e+14
```
