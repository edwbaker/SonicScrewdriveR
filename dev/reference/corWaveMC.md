# Correlate channels in a WaveMC object

Uses the corenv function from seewave to calculate the envelope
correlation for timed events between the channels of a WaveMC object

## Usage

``` r
corWaveMC(wave, times, window, cluster = NULL, ...)
```

## Arguments

- wave:

  A WaveMC object

- times:

  One or more times of events to correlate

- window:

  Width of the window to correlate in seconds (centred on times)

- cluster:

  A cluster for parallel execution

- ...:

  Additional arguments passed to
  [`seewave::corenv()`](https://rdrr.io/pkg/seewave/man/corenv.html),
  e.g. `method` or the envelope smoothing arguments.

## Value

A list with one entry per event, each holding the `corenv` list for
every channel in `correlations`, and the delay of every channel relative
to the first in `delays` (see Details).

## Details

The delays are the offsets `corenv()` reports, with the sign changed so
that a positive delay means the sound arrived at that channel later than
it arrived at the first channel, which is the convention
[`tdoa()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tdoa.md)
and
[`bearing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bearing.md)
use. `corenv()` resolves an offset only to a whole sample, and reports
any delay of zero or less as one sample greater than it is.
[`tdoa()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tdoa.md)
with `method="envelope"` measures the same thing without either
limitation, and its output can be passed to
[`bearing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bearing.md).

`corenv()` correlates ranks rather than values unless it is told
otherwise, which for a recording with quiet stretches between events can
put the largest correlation at no offset at all whatever the offset
really is. Pass `method="pearson"` if the delays look implausible.

## See also

[`tdoa()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tdoa.md),
which measures the time differences of arrival between channels, and
[`bearing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bearing.md),
which turns them into a direction.
