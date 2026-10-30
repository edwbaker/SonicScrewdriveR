# Combine annotations

Checks a list of annotations for those that are overlapping in the time
or frequency domain, and returns a list where overlapping annotations
are merged. Annotation objects must have the same `file`, `type` and
`value` to be merged, where `NA` is the same as `NA` and nothing else.

## Usage

``` r
merge_annotations(annotations, domain = "time", same.source = TRUE)
```

## Arguments

- annotations:

  A list of Annotation objects, or a single one.

- domain:

  Domain of the annotations, either "time" or "frequency".

- same.source:

  If TRUE, annotations must have the same source to be merged.

## Value

A list of Annotation objects.

## Details

The merged annotation spans all of those merged into it, in both
domains. It keeps the `source` and `metadata` of the last of them in the
order
[`sort_annotations()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/sort_annotations.md)
puts them in.
