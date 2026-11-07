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

  By default a `data.frame`. "Annotation" will return a list of
  `Annotation` objects, with the annotator as `identifiedBy` and the
  date of the annotation as `dateIdentified`; "Annotations", as it was
  spelt before, still works and is deprecated. "RecordingSet" returns
  the recordings found by a search of the recordings module as a
  [RecordingSet](https://sonicscrewdriver.ebaker.me.uk/dev/reference/recordingSet.md).
  "JSON-LD" and "Turtle" return the RDF that audioBlast gives for
  modules that describe their records as linked data, as a character
  vector with one element for each page.

- ...:

  Fields and values to filter on. Any field defined by audioBlast.

## Value

A data frame, a list of `Annotation` objects, a `RecordingSet` or a
character vector, depending on `output`. `NULL` if nothing matches the
query.

## Details

Filters are given as named arguments in `...`. audioBlast refuses a
filter that a module does not have with an error naming it, and
[`audioblastFilters()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastFilters.md)
lists the filters a module has.

A filter given `NULL` is a filter left out, as it is elsewhere in R. A
filter given `NA`, or no values at all, is refused by name instead: such
a filter is almost always a lookup that failed or a column that holds
nothing, and leaving it out would search the whole module rather than
the part that was asked for.

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

audioBlast gives the fields its records carry rather than every field
the module has, and the records of one search can come from sources that
do not hold the same fields, so the responses of a search can differ in
them. The results are joined on all the fields any response carried, and
a record that came without a field has `NA` there. The fields that only
some responses carried are named in a message, as such an `NA` says that
audioBlast did not give the field for that record rather than that its
source holds nothing.

In a `RecordingSet` each recording is identified by its audioBlast
address, and its `location` is the URL of its file. Where audioBlast has
measured a file, which its `status` says, the `hash`, `duration`,
`sample_rate`, `channels` and `codec` of the recording are what
audioBlast measured; otherwise its `duration`, `sample_rate` and
`channels` are what its source says of it. What the source says is kept
in the columns `declared_duration`, `declared_sample_rate` and
`declared_channels`, and every other field audioBlast gives is kept
under its own name. A recording that audioBlast gives without a source
or an ID has no address, and is left out with a message. A recording
given more than once, as a search paged through while recordings are
being added to audioBlast can give one, is kept once, also with a
message.

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
