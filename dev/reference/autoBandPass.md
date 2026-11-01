# Automatic Band Pass Filter

Creates an automatic bandpass filter based on the strongest frequency.
The allowed bandwidth can be an integer multiple of the bandwidth at
either -3dB or -10dB.

## Usage

``` r
autoBandPass(wave, bw = "-3dB", n.bw = 1, lowcut = 1000)
```

## Arguments

- wave:

  A Wave or WaveMC object

- bw:

  Either -3dB or -10dB. This is calculated by `frequencyStats`

- n.bw:

  The number of bandwidths either side of the centre of the centre to
  keep

- lowcut:

  Frequency (in Hz) below which the recording is ignored, which must be
  below half the sample rate. High-pass filtering is applied here before
  the centre frequency and bandwidth are calculated, and the same
  frequency is the floor of that calculation, so no band below it is
  chosen.

## Value

A band-pass filtered Wave or WaveMC object, with the channels the input
had

## Details

Every channel of a stereo `Wave` or a `WaveMC` is filtered, each around
the strongest frequency of that channel, and the result has the channels
the input had. The band is measured per channel rather than once for the
whole recording, because the channels of a recording need not hold the
same signal and measuring one of them would choose the band for all the
others.

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).

## Examples

``` r
if (FALSE) { # \dontrun{
autoBandPass(sheep)
autoBandPass(sheep, bw="-3dB", n.bw=1, lowcut=1000)
autoBandPass(sheep, bw="-10dB", n.bw=2, lowcut=0)
} # }
```
