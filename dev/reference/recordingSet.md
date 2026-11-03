# A set of recordings

A `RecordingSet` is a data frame with one row for each recording,
holding what is known of each recording without reading it. It is what
an analysis of many recordings is run over, wherever they come from.
Each place that recordings come from describes them in its own way, and
is mapped onto the same columns here, so that sets from different places
can be combined with [`rbind()`](https://rdrr.io/r/base/cbind.html).

## Usage

``` r
recordingSet(x, ...)

# Default S3 method
recordingSet(x, ...)

# S3 method for class 'character'
recordingSet(x, source = NA_character_, ...)

# S3 method for class 'data.frame'
recordingSet(x, ...)
```

## Arguments

- x:

  Paths of audio files, or a data frame with at least the columns `uri`
  and `location`.

- ...:

  Not used.

- source:

  For files, the collection they are from: one value for all of them, or
  one for each.

## Value

A `RecordingSet`.

## Details

Every set has these columns, in this order, with `NA` where a value is
not known:

- `uri`: what the recording is, unique within the set and never `NA`. A
  recording held by audioBlast has the address audioBlast gives it,
  `https://api.audioblast.org/recording/{source}/{id}`, and a file the
  `file://` address of its path.

- `source`: the collection the recording is from, e.g. "xeno-canto".

- `location`: where the audio can be read from, a path or a URL.

- `hash`: the SHA-256 hash of the file, in lower case.

- `duration`: in seconds.

- `sample_rate`: in Hz.

- `channels`: the number of channels.

- `codec`: the format the audio is in, e.g. "mp3" or "pcm_s16le".

- `status`: how measuring the file went, "ok", "missing" or
  "unreadable", or `NA` where it has not been measured.

- `start`: when the recording began, in UTC. It is given only where the
  time zone the recording was made in is known, and is never guessed.

- `tz`: the time zone the recording was made in.

- `lat`, `lon`: where the recording was made, in decimal degrees.

- `taxon`: what the recording is of.

- `author`, `license`, `rights_holder`: who made the recording, the URL
  of its licence, and who holds the rights in it. A licence that is `NA`
  is one that is not known, not one that leaves the recording free to
  use.

Any other columns a source gives follow these. An empty string in one of
the columns above is taken to be a value that is not known, and is `NA`.

Several recordings can be of one file: the Animal Sound Archive lists a
recording of several species once for each of them. Recordings that
share a hash or a location are counted as one file.

Making a set from files checks that they exist but reads nothing from
them, so their measurements are `NA`.
[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md)
with `output="RecordingSet"` gives a set of recordings held by
audioBlast, with what audioBlast has measured of their files.

Subsetting a set keeps it a set while the `uri` and `location` columns
remain, and gives a plain data frame once either is dropped. Sets are
combined with [`rbind()`](https://rdrr.io/r/base/cbind.html), with a set
as the first argument: a column that only some of the sets have is `NA`
in the rows of the others.

## See also

[`audioblast()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audioblast.md)
for a set of recordings held by audioBlast.

## Examples

``` r
f <- system.file("extdata", "AUDIOMOTH.WAV", package="sonicscrewdriver")
recordingSet(f, source="AudioMoth")
#> <RecordingSet: 1 recording of 1 file, from 1 source>
#>                                                                            uri
#>  file:///home/runner/work/_temp/Library/sonicscrewdriver/extdata/AUDIOMOTH.WAV
#>     source duration sample_rate channels status
#>  AudioMoth       NA          NA       NA   <NA>
#> Other columns: location, hash, codec, start, tz, lat, lon, taxon,
#>   author, license, rights_holder

if (FALSE) { # \dontrun{
rs <- audioblast("data", "recordings", source="bio.acousti.ca",
                 max_pages=1, output="RecordingSet")
rbind(rs, recordingSet(f))
} # }
```
