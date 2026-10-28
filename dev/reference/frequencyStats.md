# Various measurements of frequency values for a Wave object

Calculates the peak, centre, bandwidth and quality factor. The quality
factor (Q) is calculated at both -3dB and -10dB as discussed by
Bennett-Clark (1999) \<doi: 10.1080/09524622.1999.9753408\>.

## Usage

``` r
frequencyStats(
  wave,
  wave_spec = NULL,
  warn = TRUE,
  lowcut = 1000,
  plot = FALSE,
  output.unit = "kHz"
)
```

## Arguments

- wave:

  A Wave object. Stereo Wave and WaveMC objects are passed to
  allChannels(), which returns one set of statistics per channel.

- wave_spec:

  A precomputed spectrum (optional, if not present will be generated),
  with its frequencies in kHz as seewave::meanspec() returns them.

- warn:

  If TRUE provides warnings when values are not consistent

- lowcut:

  Frequency below which values are ignored, in Hz, as every other
  frequency the package takes is. This argument was in kHz in earlier
  releases: the default is the same frequency as before, but a value
  given explicitly needs multiplying by a thousand.

- plot:

  If TRUE plots the spectrum, with its frequency axis in output.unit,
  and marks the edges of the regions measured.

- output.unit:

  Unit of the frequencies and bandwidths returned, "Hz" or "kHz". The
  default is "kHz", the unit they have always been returned in, and will
  become "Hz", the unit of every other frequency the package takes or
  returns, in a future release. A call that leaves it unset warns of
  this, once per session; one that sets it, to either unit, gets the
  same results before and after that change.

## Value

A list with an entry for each of the -3dB and -10dB thresholds, and the
spectral flatness of the spectrum. Each threshold holds the frequencies
`min`, `max`, `longest_min`, `longest_max`, `peak`, `centre`,
`outer min` and `outer max` and the bandwidths `bandwidth` and
`outer bandwidth`, all in output.unit, and the quality factor `Q`. Q and
the spectral flatness are ratios, the same in either unit.
