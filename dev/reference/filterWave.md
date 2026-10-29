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

Applied to a `TaggedWave` or `TaggedWaveMC` the call is recorded in the
`processing` slot of the result, as described in
[`tagApply()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/tagApply.md).
The settings of the filter are its arguments, so two runs of the same
filter at different frequencies are told apart by their records. The
description of the filter is the process the record is under, where it
has one.
