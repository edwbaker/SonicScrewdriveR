# Convert a list of Annotation objects to a data.frame

Converts a list of Annotation objects to a data.frame, with one column
for each metadata name in the list.

## Usage

``` r
AnnotationList2DataFrame(x)
```

## Arguments

- x:

  A list of Annotation objects, or a single one.

## Value

A data.frame.

## Details

A metadata column keeps the class of its values where they share one, so
a confidence is a number; values of differing classes are given as text,
and a column in which any value is longer than one is a list. An
annotation without a value of that name has `NA`. A metadata name that
is also the name of a slot, such as the `source` that
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md)
keeps of the collection an annotation came from, is given a column of
its own named with `metadata.` in front, rather than writing over the
slot's.
