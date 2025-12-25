# Compute the acoustic activity index using scikit-maad

For addition documentation see
<https://scikit-maad.github.io/generated/maad.features.temporal_activity.html>
(Towsey 2013) .

## Usage

``` r
maad_temporal_activity(
  wave,
  dB_threshold = 3,
  mode = "fast",
  Nt = 512,
  maad = NULL
)
```

## Arguments

- wave:

  A Wave object.

- dB_threshold:

  dB threshold of activity (default = 3).

- mode:

  Mode of the envelope calculation. Can be "fast" or "Hilbert".

- Nt:

  Size of each frame. The largest, the highest is the approximation.

- maad:

  An optional maad object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

List of calculated values, comprising:

- ACTfrac:

  fraction)of points above the threshold for each frequency bin.

- ACTcount:

  total number of points above the threshold for each frequency bin.

- ACTmean:

  mean value (in dB) of the portion of the signal above the threshold.

## References

Towsey M (2013). “Noise Removal from Waveforms and Spectrograms Derived
from Natural Recordings of the Environment.” Queensland University of
Technology. <https://eprints.qut.edu.au/216373/>.
