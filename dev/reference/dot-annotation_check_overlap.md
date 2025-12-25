# Check if two annotations overlap or are continuous

Check if two annotations overlap or are continuous

## Usage

``` r
.annotation_check_overlap(annotation1, annotation2, domain = "time")
```

## Arguments

- annotation1:

  An Annotation object.

- annotation2:

  An Annotation object.

- domain:

  Domain of the annotations, either "time", "frequency", or "both".

## Value

TRUE if the annotations overlap, FALSE otherwise.
