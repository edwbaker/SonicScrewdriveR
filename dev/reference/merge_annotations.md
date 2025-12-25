# Combine annotations

Checks a list of annotations for those that are overlapping in the time
or frequency domain, and returns a list where overlapping annotations
are merged. Annotation objects must have the same `file`, `type` and
`value` to be merged.

## Usage

``` r
merge_annotations(annotations, domain = "time", same.source = TRUE)
```

## Arguments

- annotations:

  A list of Annotation objects.

- domain:

  Domain of the annotations, either "time" or "frequency".

- same.source:

  If TRUE, annotations must have the same source to be merged.

## Value

A list of Annotation objects.
