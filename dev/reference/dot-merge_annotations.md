# Combine annotations helper function

Checks a list of annotations for those that are overlapping in the time
or frequency domain, and returns a list where overlapping annotations
are merged.

## Usage

``` r
.merge_annotations(annotations, domain = "time")
```

## Arguments

- annotations:

  A list of Annotation objects.

- domain:

  Domain of the annotations, either "time" or "frequency".

## Value

A list of Annotation objects.

## Details

The exported function
[`merge_annotations()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/merge_annotations.md)
handles sanity checks and calls this function.
