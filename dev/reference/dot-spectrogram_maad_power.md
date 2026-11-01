# Helper function to convert a Wave-like object to scikit-maad spectrogram

A `spectrogram_maad` records in its `mode` slot which units its `Sxx`
holds, and the indices that want power have to be given power whatever
the caller built. Each mode is therefore converted rather than taken at
face value: an amplitude spectrogram is squared, a complex one has its
magnitude squared, and one already in dB is undone. Reading the slot is
what makes the two converters idempotent, so that a spectrogram passed
through either of them twice is not converted twice.

## Usage

``` r
.spectrogram_maad_power(object)
```

## Arguments

- object:

  A Wave or WaveMC object (or Tagged equivalent), or a spectrogram_maad
  object

## Value

A spectrogram_maad object holding power, with its mode slot saying so.
