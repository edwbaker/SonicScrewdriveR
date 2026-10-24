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
  output.unit = "bits",
  units = "decimal"
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

  Any unit accepted by
  [`convert2seconds()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/convert2seconds.md),
  which is one of seconds, minutes, hours, days, years, HHMM or POSIX.

- output.unit:

  "human", "bits" or "bytes"

- units:

  Units used when output.unit is "human", either "decimal" or "binary".
  See humanBytes().

## Value

The size of the audio file in the specified unit

## Details

The size is given in bits by default. Setting `output.unit` to "bytes"
gives bytes, and to "human" passes the result through
[`humanBytes()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/humanBytes.md),
whose choice of decimal or binary units is taken from `units`.

## Examples

``` r
# One minute of mono 16-bit audio sampled at 44.1kHz
audio_filesize(samp.rate=44100, bit.depth=16, channels=1, duration=1, duration.unit="minutes")
#> [1] 42336000

# One year of stereo 24-bit audio sampled at 96kHz
audio_filesize(samp.rate=96000, bit.depth=24, channels=2, duration=1, duration.unit="years")
#> [1] 1.453179e+14
```
