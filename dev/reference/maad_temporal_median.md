# Compute the temporal envelope median using scikit-maad

For addition documentation see
<https://scikit-maad.github.io/generated/maad.features.temporal_median.html>.

## Usage

``` r
maad_temporal_median(wave, mode = "fast", Nt = 512, maad = NULL)
```

## Arguments

- wave:

  A Wave object.

- mode:

  Mode of the envelope calculation. Can be "fast" or "hilbert".

- Nt:

  Size of each frame. The largest, the highest is the approximation.

- maad:

  An optional maad object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

Numeric median of the envelope.
