# Get data or analyses from audioBlast

Search for data or analyses on audioBlast.

## Usage

``` r
audioblast(
  type,
  name,
  endpoint = NULL,
  check = TRUE,
  max_pages = NULL,
  page = 1,
  quiet = FALSE,
  on.issue = stop,
  output = "data.frame",
  ...
)
```

## Arguments

- type:

  One of data, analysis, standalone.

- name:

  Name of data or analysis source.

- endpoint:

  Optionally specify endpoint of an audioBlast module.

- check:

  Logical. Performs sanity check on input before sending to audioBLAST.

- max_pages:

  Maximum number of data pages to return, by default this is set to NULL
  and returns all pages.

- page:

  First page of results to request, defaults to 1.

- quiet:

  If true will not print progress. Silence is a virtue.

- on.issue:

  Function to call on error or warning. By default `stop` to raise a
  standard R error. Setting to `warning` will instead a warning.

- output:

  By default a `data.frame`. "Annotations" will return a list of
  `Annotation` objects.

- ...:

  Fields and values to filter on. Any field defined by audioBLAST.

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
audioblast("data", "recordings", taxon="Gryllotalpa vineae")
} # }
```
