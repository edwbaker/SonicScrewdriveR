# A S4 class for annotations

The `Annotation` class is used to store annotations on `Wave`-like
objects.

## Details

Every slot but `metadata` holds a single value. The start of an
annotation is not after its end, nor its low frequency above its high,
and an annotation that breaks either rule is refused when it is made. A
limit that is not known is `NA`, which no rule refuses.

Who made an annotation and when are `identifiedBy` and `dateIdentified`,
as `dwc:identifiedBy` and `dwc:dateIdentified` give them for a region of
interest in Audiovisual Core. Either is `""` where it is not known. The
`source` is where the annotation came from, such as the software or the
collection, rather than who made it.

## Slots

- `file`:

  File being annotated.

- `metadata`:

  A list for storing metadata.

- `start`:

  Start time of annotation.

- `end`:

  End time of annotation.

- `low`:

  Low frequency of annotation (Hz).

- `high`:

  High frequency of annotation (Hz).

- `source`:

  Source of annotation.

- `type`:

  Type of annotation.

- `value`:

  Value of annotation.

- `identifiedBy`:

  Who made the annotation: a person, or the software and model that did.

- `dateIdentified`:

  When the annotation was made, as ISO 8601 text.
