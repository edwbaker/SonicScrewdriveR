# Computes a beat spectrum

Beat spectra represent the periodicity in signal amplitude. It is
computed by performing a continuous wavelet transform on the envelope of
a preprocessed signal, and processing the average power per frequency
band.

## Usage

``` r
beatSpectrum(wave, min_period = 0.005, max_period = 30, dj = 1/32, ...)
```

## Arguments

- wave:

  an R object or path to a wave file. Stereo Wave and WaveMC objects are
  passed to allChannels(), which returns the spectrum of each channel
  separately.

- min_period:

  the minimal rhythmicity period expected, in seconds

- max_period:

  the maximal rhythmicity period expected, in seconds, which is the
  longest period the spectrum runs to. It must be at least twice
  min_period.

- dj:

  the frequency resolution of the cwt (in voices per octave)

- ...:

  extra arguments passed to `analyze.wavelet()`

## Value

a spectrum as a data frame. It contains two columns: `power` and
`period`. The number of rows depend on the resolution and frequency
range. For multi-channel input, one such data frame per channel.

## Author

Quentin Geissmann

## Examples

``` r
if (FALSE) { # \dontrun{
beatSpectrum(sheep)
beatSpectrum(sheep, min_period=0.005, max_period=30, dj=1/32)
} # }
```
