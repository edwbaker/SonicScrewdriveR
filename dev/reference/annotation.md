# Create a new Annotation object

Create a new Annotation object

## Usage

``` r
annotation(
  file = "",
  metadata = list(),
  start = 0,
  end = Inf,
  low = 0,
  high = Inf,
  source = "",
  type = "",
  value = ""
)
```

## Arguments

- file:

  File being annotated.

- metadata:

  A list of metadata.

- start:

  Start time of annotation (seconds).

- end:

  End time of annotation (seconds).

- low:

  Low frequency of annotation (Hz).

- high:

  High frequency of annotation (Hz).

- source:

  Source of annotation.

- type:

  Type of annotation.

- value:

  Value of annotation.

## Value

An Annotation object.
