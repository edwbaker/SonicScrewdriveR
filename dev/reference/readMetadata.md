# Read every kind of metadata a file carries

Reads the embedded metadata of an audio file, whatever format that
metadata is in and whatever container the file is: GUANO, Wildlife
Acoustics wamd, Broadcast Wave bext, iXML and the INFO list from a wave
file, the AudioMoth comment, ID3 from an MP3 file or from a wave file
that holds a tag in a chunk, and Vorbis comments from a FLAC or Ogg
file. A file holding none of them gives an empty result rather than an
error. A wave file over 4 GiB, written in the RF64 or BW64 form, is read
as any other, and named by that form.

## Usage

``` r
readMetadata(x, filename = TRUE)
```

## Arguments

- x:

  Path to an audio file, a Wave object with metadata attached, or a raw
  vector holding a file

- filename:

  Whether to read the name of the file as well as its contents. TRUE
  works the format out from the name, FALSE reads only what is inside
  the file, and the name of one of
  [`parseFilename()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/parseFilename.md)'s
  formats reads the name as that, which settles a name the package could
  not have settled on its own.

## Value

An audioMetadata object. `file` and `container` are always there, so its
length says nothing about whether the file carried any metadata: ask
`length(x$sources)` for that, or `length(x$fields)` for whether anything
was read that this package has a normalised name for.

## Details

A recording commonly carries more than one of these formats, written by
the device that made it and by whatever software has handled it since,
and the same fact under a different name in each. The result therefore
holds both what each format said and one normalised record built from
all of them:

- file:

  The path the metadata was read from, or NA

- container:

  The kind of file it was read from, read from the bytes of the file
  rather than from its name, or NA where it is not one this package
  recognises

- fields:

  The normalised record (see below)

- sources:

  The formats that were found, each as the object its own reader gives,
  named `guano`, `wamd`, `audiomoth`, `bext`, `ixml`, `xenoCanto`,
  `wavInfo`, `xmp`, `disp`, `id3`, `vorbisComment` and `filename`. A
  format the file does not carry is left out.

- unparsed:

  The identifiers of any chunks the file holds that carry metadata this
  package cannot read, such as the proprietary block an editor leaves
  behind. Knowing they are there is worth more than passing over them in
  silence.

### The normalised record

`fields` holds whichever of `datetime`, `make`, `model`, `serial`,
`firmware`, `software`, `prefix`, `microphone`, `title`, `species`,
`recordist`, `comment`, `note`, `licence`, `url`, `location`,
`elevation`, `collection`, `project` and `take` the file said anything
about. `location` is a latitude and a longitude in degrees, and
`datetime` is a POSIXct wherever the file gave enough to make one.

Where two formats say the same thing, the one that is believed is the
first of GUANO, wamd, the AudioMoth comment, bext, iXML, the xeno-canto
record, the INFO list, ID3, Vorbis comments and the filename: a format
written for bioacoustics is trusted over one written for broadcast, a
recorder's own writing over a collection's transcription of what it was
told, and anything written inside the file over anything read from its
name. Nothing is thrown away by this, as `sources` still holds what
every format said.

That order says which format to believe, not which copy a writer got
right. A file can carry the same string in two places and have been
written correctly in one and badly in the other, and the record then
takes whichever the order puts first, mistake and all. One file of 972
in a cached copy of the iNaturalist corpus holds an original filename in
both its INFO list and its ID3 tag, where the recorder wrote UTF-8 into
the tag and the same text double-encoded into the list; the list
outranks the tag, so `title` comes back mangled although the file holds
a good copy. Both readings are in `sources` and neither is altered, so
where a field matters and the file carries it twice, it is worth
comparing `sources` rather than taking `fields` on its own.

A field that holds only a year gives no `datetime`, as it has no month
or day to make one with; the year can still be read from the source that
gave it. Species are taken only from a format that names a species as
such, and never guessed at from a title.

A time in an ID3 tag that names no zone is read as UTC, which version
2.4 of the format says every time in it is, rather than as a wall clock
in the zone of the session, as one in an INFO list, an XMP packet or a
Vorbis comment is read. It comes back in UTC, so it prints as the tag
wrote it whatever the zone of the session, and it may stop at the hour,
as the format allows.

### The xeno-canto record

A recording from the xeno-canto collection carries a comment the
collection wrote, beginning with its catalogue number, as
`XC428067 (c) A Recordist // Locality (1.4, 103.7), Singapore // 10m // 15:00h, 2019-02-03 // call // remarks`.
It is read into a `xenoCanto` source holding the number, recordist,
locality, latitude, longitude, country, elevation, time, date, kind of
vocalisation, remarks, whether the bird was seen and whether playback
was used, and the other species audible, and it gives the record a
`datetime`, a `location` and an `elevation` that are otherwise nowhere
in the file.

The catalogue number at the head of the comment is what marks it as one
of theirs, so a comment somebody else wrote is left alone. The number is
the recording's rather than the file's: a recording downloaded from the
collection and uploaded elsewhere keeps its comment and gains a new
name, and is read the same wherever it has got to.

What the collection was not told it writes as a placeholder rather than
leaving out, so a time of `?h`, an elevation of `?m` or a date with a
zero month or day is an absence and is read as one. A time nothing can
read costs only the `datetime`; the date is still in the source. The
species in `also:` are the others audible in the recording rather than
the one it is of, so they are not reported as its species.

### Reading from a name

`filename` reads the filename conventions that
[`parseFilename()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/parseFilename.md)
knows, and is the one source nothing outranks: where the file itself
said nothing, what its name says is the whole answer. Pass
`filename=FALSE` to read only what is inside the file.

Because nothing outranks it, a reading the name left in doubt gives no
`datetime`. A name of nothing but digits is read as a date or as a count
of seconds, and a collection that numbers its records gives names of
exactly that shape, so a date taken from one would often be a confident
wrong answer with nothing beside it to correct it.

Where you know what your own files are named by, say so: `filename`
takes the name of a filename format as well as TRUE or FALSE, as in
`filename = "YYYYMMDD"`. Naming it settles what those digits mean, so
the date is read and used rather than passed over as a guess. The
formats are those
[`parseFilename()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/parseFilename.md)
lists.

A TSA name gives a `species` and a `collection`. That corpus commonly
holds no embedded metadata at all, so for those recordings its name is
the only thing there is to read.

## See also

The readers for one format at a time:
[`guano()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/guano.md),
[`wamd()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wamd.md),
[`bext()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/bext.md),
[`ixml()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/ixml.md),
[`wavInfo()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/wavInfo.md),
[`id3()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/id3.md)
and
[`vorbisComment()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/vorbisComment.md).
The AudioMoth comment has no reader of its own any more:
[`audiomothWave()`](https://sonicscrewdriver.ebaker.me.uk/dev/reference/audiomothWave.md)
is deprecated in favour of the `audiomoth` source this returns.
`audioMetadata<-` attaches a reading to a Wave object.

## Examples

``` r
f <- system.file("extdata/AUDIOMOTH_GUANO.WAV", package="sonicscrewdriver")
m <- readMetadata(f)
m$fields$model
#> [1] "AudioMoth"
m$sources$guano
#> GUANO|Version          1.0
#> Make                   Open Acoustic Devices
#> Model                  AudioMoth
#> Serial                 247AB4055C7B17DB
#> Firmware Version       AudioMoth-Firmware-Basic (1.11.0)
#> Timestamp              2026-05-14T20:06:00Z
#> Loc Position           51.4964 -0.1224
#> OAD|Loc Source         Configuration app
#> Original Filename      20260514_210600.WAV
#> OAD|Recording Settings 8000 GAIN 2 AMP 1000 1
#> OAD|Battery Voltage    4.1
#> Temperature Int        12.3
```
