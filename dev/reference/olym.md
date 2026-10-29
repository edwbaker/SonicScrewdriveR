# Read what an Olympus recorder wrote about a recording

Reads the `olym` chunks of a wave file. Olympus recorders write one into
every file they make, holding the model of the recorder, the number the
recording was given on it, the time it was started and the time it was
ended, how long it runs, and any marks that were put in it while it was
being made.

## Usage

``` r
olym(x)
```

## Arguments

- x:

  Path to a wave file, a Wave object with olym metadata attached, a raw
  vector holding a wave file, or an olym object

## Value

An olym object, which is a named list of the fields the chunk held:
`model`, `fileNumber`, `timestamp`, `timestampEnd`, `recordingTime` and
`duration` in seconds, `marks` in seconds from the start of the
recording, and `text` and `text.2`. A file holding more than one olym
chunk gives the fields of the second under names ending in a further
number.

## Details

The chunk is commonly the only metadata such a file carries: a wave file
out of an LS or DM recorder has no INFO list, no broadcast metadata and
no tag, and before this chunk was read there was nothing in it for this
package to find.

`duration` and `recordingTime` are two ways the recorder wrote the same
thing, the first in milliseconds and the second to the second, and
across the 397 files in the corpora they never disagree. Neither is the
length of the audio the file now holds: 307 of the 397 agree with the
data chunk to within half a second, and of the 90 that do not, 89 say
the recording was longer than the audio that is there, which is what a
file cut after it came off the recorder looks like.

`timestampEnd` is not the time the audio ends either. It is the time the
recorder stopped, which includes however long it was paused for: in 152
of the 397 it is further from the start than the recording is long, by
as much as thirteen hours.

Two fields hold a hundred characters of text each, and nothing documents
what either is for. One file of the 397 writes in them, putting the
subject of the recording in the second, so they are read and reported
under the names `text` and `text.2`, by the slot they came from rather
than by any meaning, and neither is taken for the title of the
recording.

## See also

[`readMetadata()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/readMetadata.md)
reads this and every other metadata format a file carries at once, and
[`wavCues()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavCues.md)
reads the marks an editor wrote into a recording afterwards, which the
recorder's own marks are separate from.

## Examples

``` r
if (FALSE) { # \dontrun{
olym("recording.wav")
} # }
```
