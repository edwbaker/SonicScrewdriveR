# Sort annotations

Sorts a list of annotations by either start time, frequency, or both.

## Usage

``` r
sort_annotations(annotations, domain = "time", decreasing = FALSE)
```

## Arguments

- annotations:

  A list of Annotation objects, or a single one.

- domain:

  Domain of the annotations, either "time", "frequency", or "both".

- decreasing:

  If TRUE, sort in decreasing order.

## Value

A list of Annotation objects.
