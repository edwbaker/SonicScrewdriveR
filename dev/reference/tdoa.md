# Time differences of arrival between channels

Estimates the time difference of arrival (TDOA) of a sound between the
channels of a multichannel recording, by generalised cross-correlation
of each channel against a reference channel.

## Usage

``` r
tdoa(
  wave,
  ref = 1,
  from = NULL,
  to = NULL,
  units = "seconds",
  method = "phat",
  max.delay = NULL,
  interpolate = TRUE
)
```

## Arguments

- wave:

  A Wave or WaveMC object with two or more channels.

- ref:

  The channel that delays are measured against.

- from:

  Optionally, the start of the region to correlate.

- to:

  Optionally, the end of the region to correlate.

- units:

  Units in which `from` and `to` are given, one of "samples", "seconds"
  (the default), "minutes", or "hours".

- method:

  Correlation method, one of "phat" (the default), "cc", or "envelope"
  (see Details).

- max.delay:

  Optionally, the largest delay to consider, in seconds. Delays larger
  than the time sound takes to cross the microphone array are not
  physically possible, and excluding them stops a spurious correlation
  peak being returned.

- interpolate:

  If TRUE (the default) the delay is interpolated to a fraction of a
  sample (see Details).

## Value

A data frame with one row per channel, holding the channel number, the
channel name (NA if the channels are not named), the delay in seconds,
the delay in samples, and the height of the correlation peak.

## Details

A positive delay means the sound arrived at that channel later than it
arrived at the reference channel, so the reference channel is the closer
of the two to the source. The delay of the reference channel against
itself is zero.

Cross-correlation is performed over the whole recording unless `from`
and `to` are given, and a single delay is returned for each channel.
Recordings holding more than one event should be cut to each event in
turn (see Examples), as a correlation over several events with different
directions of arrival has no single peak to find. Cutting is also much
the faster way to work, as the whole of whatever is given is transformed
at once.

The following methods are available:

- **phat** (the default) - the phase transform, in which each frequency
  contributes to the correlation according to its phase alone (Knapp and
  Carter 1976) . Discarding the magnitudes sharpens the correlation peak
  and makes the estimate more robust to reverberation, at the cost of
  weighting quiet frequencies (which are mostly noise) as heavily as
  loud ones.

- **cc** - unweighted cross-correlation, normalised so that the returned
  correlation is between -1 and 1. Better suited to recordings with a
  poor signal to noise ratio, and to narrowband sounds.

- **envelope** - cross-correlation of the Hilbert amplitude envelopes of
  the channels rather than of the waveforms themselves, as
  [`seewave::corenv()`](https://rdrr.io/pkg/seewave/man/corenv.html)
  does. The only method of the three that works on a sound whose
  waveform does not stay coherent between microphones, which is the
  usual situation for the broadband pulses of insects and for
  microphones far enough apart that the path to each differs. Delays are
  resolved only to the width of a feature of the envelope, so it is the
  least precise of the three where the others work at all.

The delay found is a whole number of samples, which at the sample rates
used for field recording is a coarse measure of direction. `interpolate`
fits a parabola to the correlation peak and its two neighbours to
estimate the delay to a fraction of a sample.

## References

Knapp C, Carter GC (1976). “The generalized correlation method for
estimation of time delay.” *IEEE Transactions on Acoustics, Speech, and
Signal Processing*, **24**(4), 320–327.

## See also

[`bearing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bearing.md),
which turns the delays into a direction.

## Examples

``` r
# A noise reaching the second channel of a recording 20 samples after
# it reaches the first.
n <- tuneR::noise(kind="white", duration=5000, samp.rate=48000)
w <- tuneR::WaveMC(
  cbind(n@left[101:4900], n@left[81:4880]),
  samp.rate = 48000,
  bit = 32,
  pcm = FALSE
)
tdoa(w)
#>   channel name        delay  samples         r
#> 1       1 <NA> 0.0000000000  0.00000 0.9999390
#> 2       2 <NA> 0.0004166582 19.99959 0.9870736

# Only the middle of the recording, and only delays that an array 1m across
# could produce.
tdoa(w, from=0.02, to=0.08, max.delay=1/soundSpeed())
#>   channel name        delay  samples         r
#> 1       1 <NA> 0.0000000000  0.00000 0.9998779
#> 2       2 <NA> 0.0004166963 20.00142 0.9754278

if (FALSE) { # \dontrun{
# Delays for each of several events in a recording, each 100ms long
w <- readAudio("array.wav")
events <- c(3.2, 8.7, 12.1)
lapply(events, function(t) tdoa(w, from=t, to=t+0.1))
} # }
```
