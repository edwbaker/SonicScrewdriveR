# Read the results of a pipeline from a store

Reads what
[`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
wrote: a table for each analysis step of the pipeline, named by the
step's path through it, as
`windowing(60, time.unit = "seconds") / maad_temporal_entropy()`, in the
order of its outline. Each table holds one measurement at one grain, so
values from windows of different lengths, or from raw and filtered
audio, are never in the same table.

## Usage

``` r
pipelineResults(store, p, step = NULL)
```

## Arguments

- store:

  The folder
  [`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
  wrote to.

- p:

  The `Pipeline`, or its id. The id changes with the code of the
  packages a pipeline uses, so results written before a package was
  updated are read with the id they were written under, which is the
  name of their folder in the store.

- step:

  `NULL` for every table, or the name of one, or its number in the
  outline of the pipeline, as `"2.1"`, to read that table alone.

## Value

A named list of data frames, one for each analysis step, or a single
data frame where `step` is given. The list has a `"files"` attribute, a
data frame of every file the store holds: its `file` hash, the `uri` of
its first recording, its `location`, its `status`, `"ok"` or `"error"`,
its `error`, the `time` it was run and the `batch` it was run in.

## Details

Every table has these columns, in this order:

- `file`: the SHA-256 hash of the file the row was measured on, which
  joins to the `hash` of the set that
  [`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
  gives back.

- `pipeline`: the id of the pipeline.

- `cut_start`, `cut_end`, `window_start`, `window_end`, `window_length`:
  where in the recording the row was measured, in seconds from its
  start, for a step inside a cut or a windowing, as
  [`pipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/pipeline.md)
  describes.

- `window_time`: when the window began, where all the recordings of the
  file give the same `start`.

- `error`: why the file failed, or `NA`.

- `value` and the columns beginning `value.`: the result, as
  [`flattenOutput()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/flattenOutput.md)
  gives it.

A file that failed has one row in every table, holding only its `file`,
`pipeline` and `error`. A file in which a step found nothing has no rows
in its table, as
[`flattenOutput()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/flattenOutput.md)
gives none for `NULL`, and the `"files"` attribute tells such a file
apart from one that was never run. A table is listed once a file has run
without failing.

A file that was run more than once, by `retry = TRUE`, gives the rows of
its last run that did not fail, or its last error where every run
failed. A file at a URL that failed before its hash was known is matched
to its hash once a later run finds it.

## See also

[`runPipeline()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/runPipeline.md)
