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

  A Wave object

- bw:

  Either -3dB or -10dB. This is calculated by `frequencyStats`

- n.bw:

  The number of bandwidths either side of the centre of the centre to
  keep

- lowcut:

  High-pass filtering is applied at this frequency before calculating
  the centre frequency and bandwidth

## Value

A band-pass filtered Wave object

## Examples

``` r
if (FALSE) { # \dontrun{
autoBandPass(sheep)
autoBandPass(sheep, bw="-3dB", n.bw=1, lowcut=1000)
autoBandPass(sheep, bw="-10dB", n.bw=2, lowcut=0)
} # }
```
