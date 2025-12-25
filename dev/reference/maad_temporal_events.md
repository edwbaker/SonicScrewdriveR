# Compute the acoustic event index using scikit-maad

An acoustic event corresponds to the period of the signal above a
threshold. An acoustic event could be short (at list one point if
rejectDuration is None) or very long (the duration of the entire audio).
Two acoustic events are separated by a period with low audio signal
(i.e. below the threshold).

## Usage

``` r
maad_temporal_events(
  wave,
  dB_threshold = 3,
  rejectDuration = 0.1,
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

- rejectDuration:

  Duration of the silence to reject an acoustic event (default = 0.1).

- mode:

  Mode of the envelope calculation. Can be "fast" or "Hilbert".

- Nt:

  Size of each frame. The larger, the highest is the approximation.

- maad:

  An optional maad object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

List of calculated values, comprising:

- EVTfrac:

  fraction of points above the threshold for each frequency bin.

- EVTcount:

  total number of points above the threshold for each frequency bin.

- EVTmean:

  mean value (in dB) of the portion of the signal above the threshold.

- EVN:

  logical vector or matrix with 1 corresponding to event position.

## Details

For addition documentation see
<https://scikit-maad.github.io/generated/maad.features.temporal_events.html>
(Towsey 2013) (Towsey et al. 2018) .

## References

Towsey M (2013). “Noise Removal from Waveforms and Spectrograms Derived
from Natural Recordings of the Environment.” Queensland University of
Technology. <https://eprints.qut.edu.au/216373/>.  
  
Towsey M, Truskinger A, Cottman-Fields M, Roe P (2018). “Ecoacoustics
Audio Analysis Software.”
<https://github.com/QutEcoacoustics/audio-analysis>.
