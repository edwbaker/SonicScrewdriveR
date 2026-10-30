# Create a new Annotation object

Every argument but `metadata` is a single value. An annotation whose
start is after its end, or whose low frequency is above its high, is
refused; `NA` stands for a limit that is not known.

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
