# Read the cue points and labels of a wave file

A wave file marks positions in a recording with cue points, and names
them in a separate list, which is how an editor saves the markers and
labelled regions someone has put on a recording. This reads both and
gives them back together, one row per cue point, in the order the file
holds them.

## Usage

``` r
wavCues(x)
```

## Arguments

- x:

  Path to a wave file, or a raw vector holding one

## Value

A data frame of one row per cue point, with its identifier, position,
position in seconds, label, note, region length, purpose, region text
and the chunk its position is into. A file holding no cue points and no
labels gives a data frame of no rows.

## Details

The position of a cue point is a sample offset into the audio, counted
in frames rather than bytes, and `seconds` converts it by the sample
rate the file declares. `chunk` is the chunk the offset is into, which
is `data` for an ordinary recording; a file that keeps its audio in a
wave list can point a cue at one part of it, and then the offset is into
that part rather than into the recording.

A labelled region carries a `length` in samples and a `purpose`,
commonly `rgn`, where a plain marker carries neither. `label` is the
short name of a point, `note` a longer comment on it, and `text` the
text of a labelled region, each as the file's own list gives it.

A label whose cue point is missing is still reported, with no position:
the text is metadata the file carries, and dropping it because the chunk
that places it has been lost would be reporting less than the file
holds. A cue point with no label is likewise reported with no text; nine
files in a cached copy of bio.acousti.ca declare 99 such points, all of
them at position zero, which is a writer reserving the room rather than
marking anything.

## See also

[`wavChunks()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavChunks.md)
lists the chunks a file holds, and
[`wavInfo()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavInfo.md)
reads the text of the same labels without the positions they belong to.

## Examples

``` r
if (FALSE) { # \dontrun{
cues <- wavCues("marked.wav")
cues[!is.na(cues$label), c("seconds", "label")]
} # }
```
