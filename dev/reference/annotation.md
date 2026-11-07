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
  value = "",
  identifiedBy = "",
  dateIdentified = ""
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

- identifiedBy:

  Who made the annotation: a person, or the software and model that did.

- dateIdentified:

  When the annotation was made: ISO 8601 text, a `Date`, or a time,
  which is given as UTC. A date and time written with a space rather
  than a `T`, as `2016-10-17 13:20`, is given with a `T`.

## Value

An Annotation object.
