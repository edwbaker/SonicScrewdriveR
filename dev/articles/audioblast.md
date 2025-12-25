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
