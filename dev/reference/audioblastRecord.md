# Get one record from audioBlast

Each record held by audioBlast has its own address,
`https://api.audioblast.org/{type}/{source}/{id}`, which gives the
record as data or as RDF. As RDF, a record at its own address carries
more than the same record found by
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md),
such as the records linked to it.

## Usage

``` r
audioblastRecord(type, source, id, output = "data.frame")
```

## Arguments

- type:

  The type of the record, one of "recording", "annotation", "taxon",
  "trait", "image", "specimen", "location", "description",
  "vernacular-name", "onomatopoeia", "reference" or "link".

- source:

  The source holding the record.

- id:

  The ID of the record within its source. For an annotation this is its
  annotation ID.

- output:

  By default a `data.frame`. "JSON-LD" and "Turtle" return the record as
  RDF, in a character string.

## Value

A data frame with one row, or a character string, depending on `output`.
`NULL` if the source holds no such record.

## Examples

``` r
if (FALSE) { # \dontrun{
audioblastRecord("recording", "bio.acousti.ca", 10754)
audioblastRecord("taxon", "bio.acousti.ca", 247, output="Turtle")
} # }
```
