# Compute the temporal entropy using scikit-maad

For addition documentation see
<https://scikit-maad.github.io/generated/maad.features.temporal_entropy.html>.

## Usage

``` r
maad_temporal_entropy(
  wave,
  compatibility = "QUT",
  mode = "fast",
  Nt = 512,
  maad = NULL
)
```

## Arguments

- wave:

  A Wave object.

- compatibility:

  One of "QUT" (Towsey et al. 2018) , "seewave" (Sueur et al. 2008) .

- mode:

  Mode of the envelope calculation. Can be "fast" or "hilbert".

- Nt:

  Size of each frame. The largest, the highest is the approximation.

- maad:

  An optional maad object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

Numeric entropy of the envelope.

## References

Sueur J, Aubin T, Simonis C (2008). “Seewave, a free modular tool for
sound analysis and synthesis.” *Bioacoustics*, **18**(2), 213–226.  
  
Towsey M, Truskinger A, Cottman-Fields M, Roe P (2018). “Ecoacoustics
Audio Analysis Software.”
<https://github.com/QutEcoacoustics/audio-analysis>.
