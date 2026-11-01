# Helper function to convert a Wave-like object to a dB scikit-maad spectrogram

One already in dB is returned as it is. Converting it again would not
merely give the wrong number: dB values are commonly zero or negative,
so a second `10*log10()` returns `-Inf` and `NaN`, which then travel
into every index built on the spectrogram.

## Usage

``` r
.spectrogram_maad_dB(object)
```

## Arguments

- object:

  A Wave or WaveMC object (or Tagged equivalent), or a spectrogram_maad
  object

## Value

A spectrogram_maad object holding dB, with its mode slot saying so.
