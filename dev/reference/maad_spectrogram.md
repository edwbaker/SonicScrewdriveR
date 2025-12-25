# Compute a spectrogram of a sound wave using scikit-maad

This is a wrapper function for the `maad.sound.spectrogram` function
from the scikit-maad package for Python. It computes the spectrogram of
a sound wave. Further usage details are provided at
<https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrogram>.

## Usage

``` r
maad_spectrogram(wave, mode = "power", ..., maad = NULL)
```

## Arguments

- wave:

  A Wave object

- mode:

  The type of spectrogram to compute. Options are "power", "amplitude"
  "complex". Default is "power".

- ...:

  Additional arguments to pass to `maad.sound.spectrogram`.

- maad:

  An optional `maad` object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

Generically a `spectrogram_maad` object.
