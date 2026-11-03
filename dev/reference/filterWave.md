# Apply a WaveFilter object to a Wave object

A WaveFilter object is an object containing information necessary for
the filterw function to apply the filter to a Wave object. This is
designed to allow a pipe operator (either magrittr or base R) to be used
to apply filters to a Wave in a pipeline.

## Usage

``` r
filterWave(w, filt, cl = NULL)
```

## Arguments

- w:

  A Wave object.

- filt:

  Wave object with the selected filter applied.

- cl:

  Optional. If a cluster is specified, the filter will be applied in
  parallel.

## Value

A filtered Wave or WaveMC object, or a list of such objects if given a
list.

## Details

Supported filters include those from the seewave package.

The result has the format of the input, with the filtered samples
rounded to whole numbers where that is integer PCM. Every channel is
filtered, and eight bit PCM is filtered about its midpoint of 128.

The result is normalised to the peak of the wave it was given, every
channel by the same factor so that they keep their balance. A filter
returns its output at whatever level it gives it, and can ring past the
level it was given: a full scale square wave low passed at 1 kHz would
otherwise peak beyond what 16 bit holds, which
[`tuneR::writeWave()`](https://rdrr.io/pkg/tuneR/man/writeWave.html)
refuses. Normalised, a filtered wave fits the format it came in. A
filter keeping only part of a recording raises what it keeps to the peak
the whole recording had, so the level a band had in the input is not
kept.

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
The function the record is under is `filterWave`, as it is what filters
every channel and normalises the result, and the filter is its
arguments: first the filter's function, as `filter`, then its settings,
so that two filters with the same settings, and two runs of one filter
at different frequencies, are told apart by their records. The function
is named as it would be called from outside its package, as
`"seewave::bwfilter"`, by its name where it was written at the top
level, and as `"<anonymous function>"` where it was written inline. The
settings are named as the filter's function names them, the arguments it
was not given are recorded as its `defaults`, and the version recorded
is that of its package. The description of the filter is the process the
record is under, where it has one.

## The order of filtering and windowing

Filtering then windowing does not give the numbers windowing then
filtering does, because the result is normalised to the peak of the wave
it was given. Filtered before it is cut into windows, by
[`windowing()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/windowing.md)
or in a
[`pipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipeline.md),
the whole recording is scaled by one factor, so the level of a band can
be compared between windows, but the whole recording has to be in
memory. Filtered after, each window is scaled by a factor of its own,
raising the band in every window to that window's own peak, so the
levels of a band compared across windows mislead, but the recording can
be read a window at a time. Both orders are sound, and the record of
each filter says which was used.
