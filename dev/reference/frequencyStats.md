# Various measurements of frequency values for a Wave object

Calculates the peak, centre, bandwidth and quality factor. The quality
factor (Q) is calculated at both -3dB and -10dB as discussed by
Bennett-Clark (1999) \<doi: 10.1080/09524622.1999.9753408\>.

## Usage

``` r
frequencyStats(wave, wave_spec = NULL, warn = TRUE, lowcut = 1, plot = FALSE)
```

## Arguments

- wave:

  A Wave object

- wave_spec:

  A precomputed spectrum (optional, if not present will be generated)

- warn:

  If TRUE provides warnings when values are not consistent

- lowcut:

  Frequency (in kHz) values below which are ignored.

- plot:

  IF TRUE displays values
