# List the RIFF chunks a wave file holds

A wave file is a sequence of chunks, each named by four characters, and
what a recorder or an editor wrote into one is what the readers in this
package go looking for. This reports the chunks themselves: every chunk
the file holds, in the order it holds them, whether or not anything here
can read it.

## Usage

``` r
wavChunks(x)
```

## Arguments

- x:

  Path to a wave file, or a raw vector holding one

## Value

A data frame of one row per chunk, with the chunk's identifier, the
one-based position of the first byte of its body, its size in bytes,
whether it was truncated, and, for a `LIST` chunk, the kind of list it
is. The form of the file, its RIFF size and the path it was read from
are attached as attributes.

## Details

Only the chunk headers are read, each seeking past the body it
describes, so listing the chunks of a recording costs the same whether
it is a second long or an hour. Reading the file as a path is the
cheaper way round for that reason; a raw vector is walked where it sits.

A chunk whose declared size runs past the end of the file is reported
with the size it actually has and flagged as truncated, rather than
refused, as recordings cut short by a flat battery or a full card are
common. A file over 4 GiB written in the RF64 or BW64 form is walked by
the sizes in its `ds64` chunk, as
[`readAudio()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readAudio.md)
reads it.

## Examples

``` r
f <- system.file("extdata", "AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
wavChunks(f)
#> RIFF WAVE, 4 chunks 
#>     id start size truncated form
#> 1 fmt     21   16     FALSE <NA>
#> 2 LIST    45  436     FALSE INFO
#> 3 data   489 1600     FALSE <NA>
#> 4 guan  2097  358     FALSE <NA>

# The chunks nothing in this package reads are listed too, so a file can be
# looked at before a reader for it exists.
chunks <- wavChunks(f)
chunks[!chunks$id %in% c("fmt ", "data"), ]
#> RIFF WAVE, 2 chunks 
#>     id start size truncated form
#> 2 LIST    45  436     FALSE INFO
#> 4 guan  2097  358     FALSE <NA>
```
