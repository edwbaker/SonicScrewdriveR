# Read an Audacity label file

Reads an Audacity label file and returns either a list of `Annotation`
objects or a data frame.

## Usage

``` r
readAudacityLabels(file, output = "Annotation")
```

## Arguments

- file:

  Path to the Audacity label file.

- output:

  One of "Annotation" or "data.frame". "annotations", as it was spelt
  before, still works and is deprecated.

## Value

A list of Annotation objects, or a data frame if output is "data.frame".
