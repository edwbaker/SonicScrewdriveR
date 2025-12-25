# Compute a spectrum of a sound wave using scikit-maad

This is a wrapper function for the `maad.sound.spectrum` function. It
computes the spectrum of a sound wave. Further usage details are
provided at
<https://maad.readthedocs.io/en/latest/maad.sound.html#maad.sound.spectrum>.

## Usage

``` r
maad_spectrum(wave, ..., maad = NULL)
```

## Arguments

- wave:

  A Wave object

- ...:

  Additional arguments to pass to `maad.sound.spectrum`.

- maad:

  An optional `maad` object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

A list comprising:

- pxx:

  Power spectral density estimate.

- f_idx:

  Index of sample frequencies.
