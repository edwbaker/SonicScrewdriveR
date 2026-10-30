# Using audioBlast with R

``` r

library(sonicscrewdriver)
```

## What is audioblast?

[Audioblast](https://audioblast.org) is a discovery tool for bioacoustic
and ecoacoustic recordings. You can use it to search for recordings
(e.g. by species, location, date), annotations of recordings, analysis
results and traits.

Audioblast is currently hosted by the [Natural History
Museum](https://www.nhm.ac.uk/) in London. It was conceived by [Ed
Baker](https://ebaker.me.uk) at the [University of
York](https://www.york.ac.uk/) as part of the Leverhulme Trust funded
[Automated Acoustic Observatories](https://ebaker.me.uk/aao) project.

## Accessing audioblast

The
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md)
function of `SonicScrewdriveR` is a wrapper allowing full access to the
[Audioblast API](https://api.audioblast.org). It is also possible to
download many recordings through audioblast using
[`audioblastDownload()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastDownload.md).

## Searching

A search names the type of module (`"data"`, `"analysis"` or
`"standalone"`) and the module, and filters it with named arguments.
Each module has its own filters, and
[`audioblastFilters()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastFilters.md)
lists them. audioBlast refuses a query with a filter the module does not
have, and the error says which filters it does have.

``` r

# The filters of the recordings module
audioblastFilters("recordings")

# Recordings of a species
recs <- audioblast("data", "recordings", taxon="Gryllotalpa vineae")
```

Results come fifty to a page, and all pages are fetched unless
`max_pages` is set. A larger `page_size` fetches many results in fewer
requests.

``` r

recs <- audioblast("data", "recordings", source="bio.acousti.ca", page_size=1000)
```

A filter that takes several values can be given a vector, and matches a
row holding any one of them. Filters on a range, such as `duration`,
`lat` or `lon`, can be given a range made by
[`ab_range()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/ab_range.md).

``` r

# Two recordings at once
audioblast("data", "recordings", source="bio.acousti.ca", id=c(10754, 11096))

# Recordings between 10 and 20 seconds long
audioblast("data", "recordings", duration=ab_range(10, 20))

# Recordings made north of 60 degrees
audioblast("data", "recordings", lat=ab_range(min=60))
```

Columns that audioBlast describes as numeric, such as `duration`, are
returned as numbers.

## Records and linked data

Each record in audioBlast has its own address, and
[`audioblastRecord()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastRecord.md)
fetches one record from it. Records can also be fetched as linked data
(RDF) in JSON-LD or Turtle, from
[`audioblastRecord()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastRecord.md)
or from a search with
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md).

``` r

audioblastRecord("recording", "bio.acousti.ca", 10754)
audioblastRecord("taxon", "bio.acousti.ca", 247, output="Turtle")
```

Some modules have endpoints of their own. The classification of a taxon,
from the root of its source’s taxonomy down to the taxon itself, is
given by the `classification` endpoint of the `taxa` module.

``` r

audioblast("data", "taxa", "classification", source="bio.acousti.ca", id=247)
```

## Annotations

Annotations of recordings can be returned as `Annotation` objects, which
can be used with the other annotation functions of `SonicScrewdriveR`.

``` r

a <- audioblast("data", "annomate", source="bio.acousti.ca", source_id=10754, output="Annotation")
```

## Downloading recordings

[`audioblastDownload()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblastDownload.md)
downloads the files of recordings found by a search, and saves their
metadata alongside them. Files already downloaded are skipped. Where
audioBlast has measured a file, the file downloaded is checked against
the hash audioBlast measured.

``` r

recs <- audioblast("data", "recordings", source="inaturalist", id=134)
audioblastDownload(recs, dir="recordings")
```
