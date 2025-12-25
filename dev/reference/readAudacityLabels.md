# Read an Audacity label file

Reads an Audacity label file and returns either a list of `Annotation`
objects or a data frame.

## Usage

``` r
readAudacityLabels(file, output = "annotations")
```

## Arguments

- file:

  Path to the Audacity label file.

- output:

  One of "annotations" or "data.frame".
