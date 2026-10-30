# Read the cue points and labels of a wave file

A wave file marks positions in a recording with cue points, and names
them in a separate list, which is how an editor saves the markers and
labelled regions someone has put on a recording. This reads both and
gives them back together, one row per cue point, in the order the file
holds them.

## Usage

``` r
wavCues(x, output = "Annotation")
```

## Arguments

- x:

  Path to a wave file, or a raw vector holding one

- output:

  One of "Annotation" or "data.frame".

## Value

A list of Annotation objects, one per cue point, or with `output`
"data.frame" a data frame of one row per cue point, with its identifier,
position, position in seconds, label, note, region length, purpose,
region text and the chunk its position is into. A file holding no cue
points and no labels gives an empty list, or a data frame of no rows.

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

By default each cue point is given as an
[Annotation](https://sonicscrewdriver.ebaker.me.uk/dev/reference/Annotation-class.md)
object. A labelled region is an annotation of type `region` running from
its position for its length, and any other point an annotation of type
`cue` that starts and ends at its position. The value of the annotation
is the label of the point, or failing that the text of its region; its
source is `wavCues`, its file the path read, and its metadata the fields
of its row in the data frame other than `seconds`. An annotation whose
position is not known, or not known in seconds, starts and ends at `NA`.

## See also

[`wavChunks()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavChunks.md)
lists the chunks a file holds, and
[`wavInfo()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavInfo.md)
reads the text of the same labels without the positions they belong to.
[`writeAudacityLabels()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/writeAudacityLabels.md)
writes the annotations as an Audacity label file.

## Examples

``` r
if (FALSE) { # \dontrun{
cues <- wavCues("marked.wav", output="data.frame")
cues[!is.na(cues$label), c("seconds", "label")]
writeAudacityLabels(wavCues("marked.wav"), "marked.txt")
} # }
```
