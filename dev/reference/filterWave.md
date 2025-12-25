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

## Details

Supported filters include those from the seewave package.
