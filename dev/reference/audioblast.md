# Get data or analyses from audioBlast

Search for data or analyses on [audioBlast](https://audioblast.org).

## Usage

``` r
audioblast(
  type,
  name,
  endpoint = NULL,
  check = TRUE,
  max_pages = NULL,
  page = 1,
  page_size = 50,
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

  Logical. Checks that the module and endpoint exist before sending the
  query to audioBlast.

- max_pages:

  Maximum number of data pages to return, by default this is set to NULL
  and returns all pages.

- page:

  First page of results to request, defaults to 1.

- page_size:

  Number of results on each page, defaults to 50. Larger pages need
  fewer requests to fetch many results.

- quiet:

  If true will not print progress. Silence is a virtue.

- on.issue:

  Function to call on error or warning. By default `stop` to raise a
  standard R error. Setting to `warning` will instead a warning.

- output:

  By default a `data.frame`. "Annotations" will return a list of
  `Annotation` objects. "JSON-LD" and "Turtle" return the RDF that
  audioBlast gives for modules that describe their records as linked
  data, as a character vector with one element for each page.

- ...:

  Fields and values to filter on. Any field defined by audioBlast.

## Value

A data frame, a list of `Annotation` objects or a character vector,
depending on `output`. `NULL` if nothing matches the query.

## Details

Filters are given as named arguments in `...`. audioBlast refuses a
filter that a module does not have with an error naming it, and
[`audioblastFilters()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastFilters.md)
lists the filters a module has.

A filter that audioBlast lists as taking several values can be given a
vector, e.g. `id = c(10754, 11096)`, which matches a row holding any one
of them. audioBlast takes at most 100 values in a request, so longer
vectors are sent as several requests and their results combined.

A filter on a range (e.g. `duration` or `lat`) can be given a single
value, matched exactly, or a range made with
[`ab_range()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/ab_range.md),
e.g. `duration = ab_range(10, 20)`.

Columns that audioBlast describes as numeric are returned as numbers,
and all others as character strings.

## See also

[`audioblastFilters()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastFilters.md)
for the filters a module takes,
[`audioblastRecord()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastRecord.md)
to fetch one record, and
[`audioblastDownload()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastDownload.md)
to download the recordings found.

## Examples

``` r
if (FALSE) { # \dontrun{
audioblast("data", "recordings", taxon="Gryllotalpa vineae")

# Several recordings at once
audioblast("data", "recordings", source="bio.acousti.ca", id=c(10754, 11096))

# Recordings between 10 and 20 seconds long
audioblast("data", "recordings", source="bio.acousti.ca",
           duration=ab_range(10, 20), max_pages=1)

# The classification of a taxon, from the root down
audioblast("data", "taxa", "classification", source="bio.acousti.ca", id=247)
} # }
```
