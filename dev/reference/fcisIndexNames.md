# Per-frequency-bin acoustic indices

The indices available as channels of a false-colour index spectrogram.
Each is calculated separately for every frequency bin of a spectrogram,
giving one value per bin rather than one value for the whole signal.

## Usage

``` r
fcisIndexNames()
```

## Value

A character vector of index names.

## Details

- power:

  Mean of the squared amplitude.

- ACI:

  Acoustic complexity index, the summed absolute difference between
  consecutive time frames, divided by the total amplitude in the bin.

- entropy:

  Temporal entropy of the amplitude, scaled to between 0 and 1. A bin
  whose amplitude is constant over time has an entropy of 1.

- background:

  An estimate of the background level, taken as the tenth percentile of
  the amplitude over time.

- cover:

  The proportion of time frames whose amplitude is more than three times
  the background level of the bin.

## Examples

``` r
fcisIndexNames()
#> [1] "power"      "ACI"        "entropy"    "background" "cover"     
```
