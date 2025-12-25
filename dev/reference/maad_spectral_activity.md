# Compute the spectral activity using scikit-maad

Acoustic activity corresponds to the portion of the spectrogram above a
threshold frequency per frequency along time axis (Towsey 2017) .

## Usage

``` r
maad_spectral_activity(object, dB_threshold = 6, maad = NULL)
```

## Arguments

- object:

  A Wave object or a spectrogram_maad object.

- dB_threshold:

  dB threshold of activity (default = 6).

- maad:

  An optional maad object. If not provided, one will be created using
  [`getMaad()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/getMaad.md).

## Value

The function computes for each frequency bin:

- ACTfract:

  Proportion (fraction) of points above the threshold.

- ACTcount:

  Total number of points above the threshold.

- ACTmean:

  Mean value (in dB) of the portion of the signal above the threshold.

## References

Towsey M (2017). “The calculation of acoustic indices derived from
long-duration recordings of the natural environment.” Queensland
University of Technology.
<https://eprints.qut.edu.au/110634/1/QUTePrints110634_TechReport_Towsey2017August_AcousticIndices%20v3.pdf>.
